{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
module GHC.Eventlog.Machines (
    -- * Machines
    sourceHandleWait,
    decodeEventsMaybe,
    reorderEvents,
    checkOrder,

    -- * Exceptions
    DecodeError (..),
) where

import Control.Exception          (Exception, throwIO, catch)
import System.IO.Error (isEOFError)
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Class  (lift)
import Data.Foldable              (traverse_)
import Data.Function              (fix)
import Data.Int                   (Int64)
import Data.List                  (partition, sortBy)
import Data.Ord                   (comparing)
import Data.Word                  (Word64)
import GHC.RTS.Events             (Event, Timestamp, evTime)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import System.IO                  (Handle, hWaitForInput)

import Data.Machine (Is, MachineT, PlanT, ProcessT, await, construct, yield)

import qualified Data.ByteString as BS
import qualified System.Clock    as Clock

-------------------------------------------------------------------------------
-- Reading from a handle
-------------------------------------------------------------------------------

-- | A source which waits for input using 'hWaitForInput',
-- produces 'Nothing' events on timeout.
sourceHandleWait
    :: MonadIO m
    => Handle
    -> Int  -- ^ wait timeout (in microseconds, argument of 'hWaitForInput'')
    -> Int  -- ^ number of bytes to read (argument of 'BS.hGetSome')
    -> MachineT m k (Maybe BS.ByteString)
sourceHandleWait hdl timeout size = construct $ fix $ \loop -> do
    ready <- liftIO $ hWaitForInput' hdl timeout
    case ready of
        Ready -> do
            bs <- liftIO $ BS.hGetSome hdl size
            yield (Just bs)
            loop
        NotReady -> do
            yield Nothing
            loop
        EOF ->
            return ()

data Ready
    = Ready
    | EOF
    | NotReady

hWaitForInput' :: Handle -> Int -> IO Ready
hWaitForInput' hdl timeout = catch (fmap f (hWaitForInput hdl timeout)) g where
    f True  = Ready
    f False = NotReady

    g exc | isEOFError exc = return EOF
          | otherwise      = throwIO exc

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

-- | Parse 'Event's from a stream of 'BS.ByteString' chunks.
--
-- Throws 'DecodeError' on error.
--
decodeEventsMaybe :: MonadIO m => ProcessT m (Maybe BS.ByteString) (Maybe Event)
decodeEventsMaybe = construct $ loop decodeEventLog where
    loop :: MonadIO m => Decoder a -> PlanT (Is (Maybe BS.ByteString)) (Maybe a) m ()
    loop Done {} =
        return ()

    loop (Consume k) = do
        -- yield Nothing, so we keep producing ticks
        mbs <- await
        case mbs of
            Nothing -> loop (Consume k)
            Just bs -> loop (k bs)

    loop (Produce a d') = do
        yield (Just a)
        loop d'

    loop (Error _ err) =
        liftIO $ throwIO $ DecodeError err

newtype DecodeError = DecodeError String deriving Show
instance Exception DecodeError

-------------------------------------------------------------------------------
-- Reordering buffer.
-------------------------------------------------------------------------------

-- | Buffer and reorder 'Event's to hopefully achieve
-- monotonic, causal stream of 'Event's.
--
reorderEvents :: MonadIO m => Word64 -> ProcessT m (Maybe Event) Event
reorderEvents interval = construct start where
    -- interval to wait for cutoff, 1.5 of flushout interval
    interval' :: Int64
    interval' = fromIntegral $ interval + interval `div` 2

    start :: MonadIO m => PlanT (Is (Maybe Event)) Event m ()
    start = do
        mev <- await
        case mev of
            Nothing -> start
            Just ev -> do
                our <- liftIO $ fmap timeSpec $ Clock.getTime Clock.Monotonic
                loop (our - timeStamp (evTime ev)) [ev]

    -- the Int64 argument is the minimum difference we have seen between our
    -- clock and incoming events.
    loop :: MonadIO m => Int64 -> [Event] -> PlanT (Is (Maybe Event)) Event m ()
    loop diff evs = do
        mev <- await
        case mev of
            Nothing -> do
                our  <- liftIO $ fmap timeSpec $ Clock.getTime Clock.Monotonic
                yieldEvents our diff evs

            Just ev -> do
                our  <- liftIO $ fmap timeSpec $ Clock.getTime Clock.Monotonic

                -- Adjust the difference
                let this :: Int64
                    this = our - timeStamp (evTime ev)

                let diff' | abs diff < abs this = diff
                          | otherwise           = this

                yieldEvents our diff' (ev : evs)

    yieldEvents :: MonadIO m => Int64 -> Int64 -> [Event] -> PlanT (Is (Maybe Event)) Event m ()
    yieldEvents our diff evs = do
        -- approximation of events time in our clock
        let approx e = timeStamp (evTime e) + diff

        let cutoff = our - interval'
        let (old, new) = partition (\e -> approx e < cutoff) evs
        traverse_ yield (sortBy (comparing evTime) old)
        loop diff new

    timeStamp :: Timestamp -> Int64
    timeStamp = fromIntegral

    timeSpec :: Clock.TimeSpec -> Int64
    timeSpec ts
        | ns >= 0   = fromIntegral ns
        | otherwise = 0
      where ns = Clock.sec ts * 1_000_000_000 + Clock.nsec ts

-- | Machine which checks that consecutive events are properly ordered.
-- Runs an effect on non-causal events.
checkOrder
    :: Monad m
    => (Event -> Event -> m ())
    -> ProcessT m Event Event
checkOrder f = construct start where
    start = do
        e <- await
        yield e
        loop e

    loop e = do
        e' <- await
        when (evTime e' < evTime e) $ lift $ f e e'
        yield e'
        loop e'
