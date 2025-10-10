{- |
Module      : GHC.Eventlog.Live.Machine.Decoder
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Decoder (
  -- * Event decoding
  DecodeError (..),
  decodeEvent,
  decodeEventBatch,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Machine (Is, PlanT, ProcessT, await, construct, yield)
import GHC.Eventlog.Live.Machine.Core (Tick (..), liftTick)
import GHC.RTS.Events (Event)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)

-------------------------------------------------------------------------------
-- Decoding events

{- |
Parse t'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws a t'DecodeError' on error.
-}
decodeEvent :: (MonadIO m) => ProcessT m BS.ByteString Event
decodeEvent = construct $ loop decodeEventLog
 where
  loop :: (MonadIO m) => Decoder a -> PlanT (Is BS.ByteString) a m ()
  loop Done{} = pure ()
  loop (Consume k) = await >>= \chunk -> loop (k chunk)
  loop (Produce a d') = yield a >> loop d'
  loop (Error _ err) = liftIO $ throwIO $ DecodeError err

{- |
Parse 'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws 'DecodeError' on error.
-}
decodeEventBatch :: (MonadIO m) => ProcessT m (Tick BS.ByteString) (Tick Event)
decodeEventBatch = liftTick decodeEvent

newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError
