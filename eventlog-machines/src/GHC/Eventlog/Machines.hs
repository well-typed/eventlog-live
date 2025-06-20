{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
module GHC.Eventlog.Machines (
    -- * Machines
    events,
    sourceHandleWait,
    decodeEventsMaybe,
    reorderEvents,
    checkOrder,

    -- ** Delimiting
    delimit,
    between,

    -- * Exceptions
    DecodeError (..),
) where

import Control.Exception          (Exception, catch, throwIO)
import Control.Monad              (when)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Class  (lift)
import Data.Foldable              (traverse_)
import Data.Function              (fix)
import Data.Int                   (Int64)
import Data.List                  (partition, sortBy)
import Data.Machine.Moore         (Moore (..))
import Data.Ord                   (comparing)
import Data.Text                  (Text)
import Data.Word                  (Word64)
import GHC.RTS.Events
       (Event (evSpec), EventInfo (UserMarker), Timestamp, evTime)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import System.IO                  (Handle, hWaitForInput)
import System.IO.Error            (isEOFError)
import Data.Machine (Is, MachineT, PlanT, ProcessT, await, construct, repeatedly, yield, (~>))
import qualified Data.ByteString as BS
import qualified System.Clock    as Clock

-------------------------------------------------------------------------------
-- Event source
-------------------------------------------------------------------------------

events ::
    (MonadIO m) =>
    Handle ->
    Int ->
    Int ->
    Word64 ->
    (Event -> Event -> m ()) ->
    MachineT m k Event
events handle timeout chunkSize interval nonCausalAction =
  sourceHandleWait handle timeout chunkSize
    ~> decodeEventsMaybe
    ~> reorderEventsMaybe
    ~> checkOrder nonCausalAction
 where
  reorderEventsMaybe
    | interval == 0 = forwardEventsMaybe
    | otherwise = reorderEvents interval

  forwardEventsMaybe :: Monad m => ProcessT m (Maybe a) a
  forwardEventsMaybe = let go = await >>= maybe go yield in repeatedly go

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

-------------------------------------------------------------------------------
-- Filtering semaphores
-------------------------------------------------------------------------------

-- | A simple delimiting 'Moore' machine,
-- which is opened by one constant marker and closed by the other one.
between :: Text -> Text -> Moore Text Bool
between x y = open where
    open  = Moore False open' where open' x' = if x == x' then close else open
    close = Moore True close' where close' y' = if y == y' then end else close
    end   = Moore False (const end)

-- | Delimit the event process.
delimit :: Monad m => Moore Text Bool -> ProcessT m Event Event
delimit = construct . go where
    go :: Monad m => Moore Text Bool -> PlanT (Is Event) Event m ()
    go mm@(Moore s next) = do
        e <- await
        case evSpec e of
            -- on marker step the moore machine.
            UserMarker m -> do
                let mm'@(Moore s' _) = next m
                -- if current or next state is open (== True), emit the marker.
                when (s || s') $ yield e
                go mm'

            -- for other events, emit if the state is open.
            _ -> do
                when s $ yield e
                go mm
