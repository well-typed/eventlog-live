{- |
Module      : GHC.Eventlog.Live.Machine.Analysis.InfoProv
Description : Machine for gathering info table provenance information.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.InfoProv (
  InfoProvTable,
  withInfoProvTable,
  save,
  indexing,
  lookup,
  lookups,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.Machine (Process, ProcessT, await, buffered, construct, mapping, repeatedly, yield, (~>))
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Eventlog.Live.Database (Session, Table, TableOptions (..))
import GHC.Eventlog.Live.Database qualified as DB
import GHC.Eventlog.Live.Data.InfoProv (InfoProv (..), InfoProvPtr (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import Prelude hiding (lookup)

{- |
Representation of an `InfoProv` table.
-}
newtype InfoProvTable = InfoProvTable (Table (DB.SerialiseVia InfoProvPtr Word64) (DB.SerialiseViaBinary InfoProv))

{- |
Create an empty `InfoProv` table.

If the first argument is @`Just` tableFilePath@, the table is loaded from @tableFilePath@.
Otherwise, an empty table is created.
-}
withInfoProvTable :: Logger IO -> Session -> Maybe FilePath -> (InfoProvTable -> IO a) -> IO a
withInfoProvTable logger session maybeInfoProvTableFilePath action = do
  -- Create the table options.
  let tableOptions =
        LSMTreeTableOptions
          { tableName = "info-prov-table"
          , tableLabel = "InfoProvPtr-InfoProv"
          , maybeTableFilePath = maybeInfoProvTableFilePath
          }
  -- Create the table.
  DB.withTable logger session tableOptions $ action . InfoProvTable

{- |
Save an `InfoProv` table to a file.
-}
save :: Logger IO -> InfoProvTable -> FilePath -> IO ()
save = coerce DB.saveTable

{- |
Resolve `InfoProvPtr` keys to `InfoProv` values from an `InfoProvTable`.
-}
lookups :: InfoProvTable -> Vector InfoProvPtr -> IO (Vector (Maybe InfoProv))
lookups = coerce DB.lookups

{- |
Resolve an `InfoProvPtr` key to a `InfoProv` value from an `InfoProvTable`.
-}
lookup :: InfoProvTable -> InfoProvPtr -> IO (Maybe InfoProv)
lookup = coerce DB.lookup

{- |
Insert @(`InfoProvPtr`, `InfoProv`)@ entries into an `InfoProvTable`.
-}
inserts :: InfoProvTable -> Vector (InfoProvPtr, InfoProv) -> IO ()
inserts = coerce DB.inserts

{- |
Index `InfoProv` entries from a GHC event stream into an `InfoProvTable`.
-}
indexing ::
  InfoProvTable ->
  -- | The buffer size. Defaults to 10.
  Maybe Int ->
  ProcessT IO Event Void
indexing infoProvTable maybeBufferSize =
  extractInfoProv
    ~> buffered (fromMaybe 10 maybeBufferSize)
    ~> mapping V.fromList
    ~> repeatedly (await >>= liftIO . inserts infoProvTable)

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

        -- If the event is NOT an `E.InfoTableProv` evenDB...
        | otherwise ->
            -- ...and we have started...
            if started
              then pure () -- ...stop.
              else go started -- ...otherwise, continue.
