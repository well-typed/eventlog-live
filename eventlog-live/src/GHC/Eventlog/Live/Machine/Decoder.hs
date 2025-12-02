{- |
Module      : GHC.Eventlog.Live.Machine.Decoder
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Decoder (
  -- * Event decoding
  decodeEvent,
  decodeEventBatch,
) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString qualified as BS
import Data.Machine (Is, PlanT, ProcessT, await, construct, yield)
import Data.Text qualified as T
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (LogAction, (&>), (<&))
import GHC.Eventlog.Live.Machine.Core (Tick (..), liftTick)
import GHC.RTS.Events (Event)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)

-------------------------------------------------------------------------------
-- Decoding events

{- |
Parse t'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws a t'DecodeError' on error.
-}
decodeEvent ::
  forall m.
  (Monad m) =>
  LogAction m ->
  ProcessT m BS.ByteString Event
decodeEvent logAction = construct $ loop decodeEventLog
 where
  loop :: Decoder a -> PlanT (Is BS.ByteString) a m ()
  loop Done{} = pure ()
  loop (Consume k) = await >>= \chunk -> loop (k chunk)
  loop (Produce a d') = yield a >> loop d'
  loop (Error _ err) = lift $ logAction <& ERROR &> T.pack err

{- |
Parse 'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws 'DecodeError' on error.
-}
decodeEventBatch ::
  (Monad m) =>
  LogAction m ->
  ProcessT m (Tick BS.ByteString) (Tick Event)
decodeEventBatch logAction = liftTick $ decodeEvent logAction
