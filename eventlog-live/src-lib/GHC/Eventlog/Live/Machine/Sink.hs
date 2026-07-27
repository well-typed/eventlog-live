{- |
Module      : GHC.Eventlog.Live.Machine.Sink
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Sink (
  -- * Eventlog file sink
  fileSink,
  fileSinkBatch,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Machine (ProcessT, await, repeatedly, (~>))
import Data.Void (Void)
import GHC.Eventlog.Live.Machine.Core (Tick (..), dropTick)
import System.IO (Handle)

-------------------------------------------------------------------------------
-- Log file sink

{- |
File sink for optional eventlog log file.
-}
fileSink ::
  (MonadIO m) =>
  Handle ->
  ProcessT m BS.ByteString Void
fileSink handle = repeatedly $ await >>= liftIO . BS.hPut handle

-------------------------------------------------------------------------------
-- Log file sink with batches

{- |
File sink for optional eventlog log file.
-}
fileSinkBatch ::
  (MonadIO m) =>
  Handle ->
  ProcessT m (Tick BS.ByteString) Void
fileSinkBatch handle = dropTick ~> fileSink handle
