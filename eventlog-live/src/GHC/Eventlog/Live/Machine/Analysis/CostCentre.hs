{- |
Module      : GHC.Eventlog.Live.Machine.Analysis.CostCentre
Description : Machine for gathering info table provenance information.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.CostCentre (
  CostCentreTable,
  withCostCentreTable,
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
import Data.Word (Word32)
import GHC.Eventlog.Live.Database (Session, Table, TableOptions (..))
import GHC.Eventlog.Live.Database qualified as DB
import Foreign (toBool)
import Foreign.C.Types (CBool (..))
import GHC.Eventlog.Live.Data.CostCentre (CostCentre (..), CostCentreId (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import Prelude hiding (lookup)

{- |
Representation of an `CostCentre` table.
-}
newtype CostCentreTable = CostCentreTable (Table (DB.SerialiseVia CostCentreId Word32) (DB.SerialiseViaBinary CostCentre))

{- |
Create an empty `CostCentre` table.

If the first argument is @`Just` tableFilePath@, the table is loaded from @tableFilePath@.
Otherwise, an empty table is created.
-}
withCostCentreTable :: Logger IO -> Session -> Maybe FilePath -> (CostCentreTable -> IO a) -> IO a
withCostCentreTable logger session maybeCostCentreTableFilePath action = do
  -- Create the table options.
  let tableOptions =
        LSMTreeTableOptions
          { tableName = "cost-centre-table"
          , tableLabel = "CostCentreId-CostCentre"
          , maybeTableFilePath = maybeCostCentreTableFilePath
          }
  -- Create the table.
  DB.withTable logger session tableOptions $ action . CostCentreTable

{- |
Save an `CostCentre` table to a file.
-}
save :: Logger IO -> CostCentreTable -> FilePath -> IO ()
save = coerce DB.saveTable

{- |
Resolve `CostCentreId` keys to `CostCentre` values from an `CostCentreTable`.
-}
lookups :: CostCentreTable -> Vector CostCentreId -> IO (Vector (Maybe CostCentre))
lookups = coerce DB.lookups

{- |
Resolve an `CostCentreId` key to a `CostCentre` value from an `CostCentreTable`.
-}
lookup :: CostCentreTable -> CostCentreId -> IO (Maybe CostCentre)
lookup = coerce DB.lookup

{- |
Insert @(`CostCentreId`, `CostCentre`)@ entries into an `CostCentreTable`.
-}
inserts :: CostCentreTable -> Vector (CostCentreId, CostCentre) -> IO ()
inserts = coerce DB.inserts

{- |
Index `CostCentre` entries from a GHC event stream into an `CostCentreTable`.
-}
indexing ::
  CostCentreTable ->
  -- | The buffer size. Defaults to 10.
  Maybe Int ->
  ProcessT IO Event Void
indexing infoProvTable maybeBufferSize =
  extractCostCentre
    ~> buffered (fromMaybe 10 maybeBufferSize)
    ~> mapping V.fromList
    ~> repeatedly (await >>= liftIO . inserts infoProvTable)

{- |
Extract `CostCentre` entries from a stream of GHC events.

This machine starts yielding `CostCentre` entries when the first
`E.InfoTableProv` event is received, and stops altogether once
the first subsequent non-`E.InfoTableProv` event is received.
-}
extractCostCentre :: Process Event (CostCentreId, CostCentre)
extractCostCentre = construct $ go False
 where
  go started =
    await >>= \case
      i
        -- If the event is an `E.InfoTableProv` event, process it, and set @started@...
        | E.HeapProfCostCentre{..} <- i.evSpec -> do
            let ccId = CostCentreId heapProfCostCentreId
            let cc =
                  CostCentre
                    { ccLabel = heapProfLabel
                    , ccModule = heapProfModule
                    , ccSrcLoc = heapProfSrcLoc
                    , ccIsCAF = toBool (coerce @_ @CBool heapProfFlags)
                    }
            yield (ccId, cc)
            go True

        -- If the event is NOT an `E.HeapProfCostCentre` evenDB...
        | otherwise ->
            -- ...and we have started...
            if started
              then pure () -- ...stop.
              else go started -- ...otherwise, continue.
