{- |
Module      : GHC.Eventlog.Live.Processor.Logs
Description : Log Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Logs (
  processLogEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Data (Proxy (..))
import Data.Machine (Process, ProcessT, mapping, (~>))
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Data.Logs (SomeLogs (..))
import GHC.Eventlog.Live.Machine.Analysis.Log qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Processor.Core.Logs (process)
import GHC.RTS.Events (Event (..))

--------------------------------------------------------------------------------
-- processLogEvents
--------------------------------------------------------------------------------

processLogEvents ::
  (MonadIO m) =>
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList SomeLogs))
processLogEvents fullConfig =
  M.fanoutTick
    [ processThreadLabel fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processUserMarker fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processUserMessage fullConfig
        ~> M.liftTick (mapping D.singleton)
    ]

--------------------------------------------------------------------------------
-- UserMessage

processUserMessage :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeLogs)
processUserMessage =
  process (Proxy @C.UserMessageLog) M.processUserMessage

--------------------------------------------------------------------------------
-- UserMarker

processUserMarker :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeLogs)
processUserMarker =
  process (Proxy @C.UserMarkerLog) M.processUserMarker

--------------------------------------------------------------------------------
-- ThreadLabel

processThreadLabel :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeLogs)
processThreadLabel =
  process (Proxy @C.ThreadLabelLog) M.processThreadLabel
