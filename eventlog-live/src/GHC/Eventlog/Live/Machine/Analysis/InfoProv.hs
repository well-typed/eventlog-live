{- |
Module      : GHC.Eventlog.Live.Machine.Analysis.InfoProv
Description : Machine for gathering info table provenance information.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.InfoProv (
  withNewInfoProvDatabase,
  InfoProvDatabase,
  indexInfoProv,
  lookupInfoProv,
  lookupInfoProvs,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Machine (Process, ProcessT, await, buffered, construct, mapping, repeatedly, yield, (~>))
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Database.LSMTree qualified as LSMT
import GHC.Eventlog.Live.Data.InfoProv (InfoProv (..), InfoProvPtr (..))
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import System.FS.API (HasFS (..))
import System.FS.API.Strict qualified as FS
import System.FS.IO qualified as FS.IO
import System.IO.Temp (withSystemTempDirectory)

{- |
An `InfoProv` store.
-}
newtype InfoProvDatabase = InfoProvDatabase
  { infoProvTable :: LSMT.Table IO InfoProvPtr InfoProv Void
  }

{- |
Create an empty `InfoProvDatabase`.
-}
withNewInfoProvDatabase :: Maybe String -> (InfoProvDatabase -> IO a) -> IO a
withNewInfoProvDatabase maybeLabel action = do
  -- Create a temporary directory for the LSM Tree.
  let !label = fromMaybe "eventlog-live-InfoProvDatabase" maybeLabel
  withSystemTempDirectory label $ \storeDir -> do
    -- Open the LSM Tree session.
    let mountPoint = FS.MountPoint storeDir
    let sessionDirFsPath = FS.mkFsPath ["session"]
    let hasFS = FS.IO.ioHasFS @IO mountPoint
    createDirectoryIfMissing hasFS True sessionDirFsPath
    LSMT.withOpenMountedSessionIO mempty storeDir sessionDirFsPath $ \session -> do
      -- Create a new LSM Tree Table.
      LSMT.withTable session $ \infoProvTable -> do
        -- Run the action.
        action InfoProvDatabase{..}

{- |
Resolve `InfoProvPtr` keys to `InfoProv` values from an `InfoProvDatabase`.
-}
lookupInfoProvs ::
  InfoProvDatabase ->
  Vector InfoProvPtr ->
  IO (Vector (Maybe InfoProv))
lookupInfoProvs InfoProvDatabase{..} infoProvPtrs =
  fmap LSMT.getValue <$> LSMT.lookups infoProvTable infoProvPtrs

{- |
Resolve an `InfoProvPtr` key to a `InfoProv` value from an `InfoProvDatabase`.
-}
lookupInfoProv ::
  InfoProvDatabase ->
  InfoProvPtr ->
  IO (Maybe InfoProv)
lookupInfoProv InfoProvDatabase{..} infoProvPtr =
  LSMT.getValue <$> LSMT.lookup infoProvTable infoProvPtr

{- |
Index `InfoProv` entries from a GHC event stream into an `InfoProvDatabase`.
-}
indexInfoProv ::
  InfoProvDatabase ->
  -- | The buffer size. Defaults to 10.
  Maybe Int ->
  ProcessT IO Event Void
indexInfoProv InfoProvDatabase{..} maybeBufferSize =
  extractInfoProv
    ~> buffered (fromMaybe 10 maybeBufferSize)
    ~> mapping (V.fromList . fmap (\(ipPtr, ip) -> (ipPtr, ip, Nothing)))
    ~> repeatedly (await >>= liftIO . LSMT.inserts infoProvTable)

{- |
Extract `InfoProv` entries from a stream of GHC events.

This machine starts yielding `InfoProv` entries when the first
`E.InfoTableProv` event is received, and stops altogether once
the first subsequent non-`E.InfoTableProv` event is received.
-}
extractInfoProv :: Process Event (InfoProvPtr, InfoProv)
extractInfoProv = construct $ go False
 where
  go started =
    await >>= \case
      i
        -- If the event is an `E.InfoTableProv` event, process it, and set @started@...
        | E.InfoTableProv{..} <- i.evSpec -> do
            let !ipPtr = InfoProvPtr itInfo
            let !ip =
                  InfoProv
                    { ipName = itTableName
                    , ipClosureDesc = itClosureDesc
                    , ipTyDesc = itTyDesc
                    , ipLabel = itLabel
                    , ipModule = itModule
                    , ipSrcLoc = itSrcLoc
                    }
            yield (ipPtr, ip)
            go True

        -- If the event is NOT an `E.InfoTableProv` event...
        | otherwise ->
            -- ...and we have started...
            if started
              then pure () -- ...stop.
              else go started -- ...otherwise, continue.
