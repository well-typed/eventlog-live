module Database.LSMTree.Compat (
  Table,
  TableOptions (..),
  withTable,
  saveTable,
  lookup,
  lookups,
  inserts,
  SerialiseViaBinary (..),
  SerialiseVia (..),
) where

import Control.Exception (Exception (..), IOException, bracket_, catch, throwIO)
import Control.Monad (unless, when)
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.Coerce (Coercible, coerce)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Void (Void)
import Database.LSMTree qualified as LSMT
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import System.Directory qualified as SD
import System.FS.API.Strict qualified as FS
import System.FS.BlockIO.IO qualified as BIO
import System.FilePath ((</>))
import System.FilePath qualified as SF
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import Prelude hiding (lookup)

{- |
Representation of tables.
-}
type Table :: Type -> Type -> Type
data Table k v
  = (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v) =>
  LSMTreeTable
  { mountPoint :: FS.MountPoint
  , sessionRoot :: FS.FsPath
  , session :: LSMT.Session IO
  , table :: LSMT.Table IO k v Void
  , tableName :: String
  , snapshotName :: LSMT.SnapshotName
  , snapshotLabel :: LSMT.SnapshotLabel
  }

{- |
Internal helper.

Representation of sessions.
-}
type Session :: Type
data Session
  = LSMTreeSession
  { mountPoint :: FS.MountPoint
  , sessionRoot :: FS.FsPath
  , session :: LSMT.Session IO
  , snapshotName :: LSMT.SnapshotName
  , snapshotLabel :: LSMT.SnapshotLabel
  }

{- |
The options for tables.
-}
type TableOptions :: Type -> Type -> Type
data TableOptions k v
  = (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v) => LSMTreeTableOptions
  { tableName :: String
  , tableLabel :: String
  , maybeTableFilePath :: Maybe FilePath
  , maybeSessionRoot :: Maybe FilePath
  }

newtype TargetExistsError = TargetExistsError FilePath
  deriving stock (Show)

instance Exception TargetExistsError

{- |
Run an action with a table.

If @`TableOptions`.`maybeTableFilePath`@ is set, the table is imported from the given snapshot.
-}
withTable :: Logger IO -> TableOptions k v -> (Table k v -> IO a) -> IO a
withTable logger tableOptions@LSMTreeTableOptions{..} action = do
  withNewSession logger tableOptions $ \LSMTreeSession{..} -> do
    case maybeTableFilePath of
      Nothing ->
        -- Create a new LSM Tree table.
        LSMT.withTable session $ \table ->
          -- Run the action.
          action LSMTreeTable{..}
      Just tableRelFilePath -> do
        -- Find the absolute file path to the table.
        tableAbsFilePath <- SD.makeAbsolute tableRelFilePath

        -- Load the LSM Tree table.
        let loadSnapshot :: IO ()
            loadSnapshot = do
              success <- tryLoadTableByHardlink
              unless success loadTableByCopy

            tryLoadTableByHardlink :: IO Bool
            tryLoadTableByHardlink = do
              writeLog logger DEBUG . T.pack $
                "Trying to load table " <> tableName <> " by hardlinking from " <> tableAbsFilePath
              success <-
                case FS.fsFromFilePath mountPoint tableAbsFilePath of
                  Nothing -> do
                    writeLog logger DEBUG . T.pack $
                      let FS.MountPoint mountPointPath = mountPoint
                       in "Could not load table " <> tableName <> " by hardlinking from " <> tableAbsFilePath <> ": not under mount point " <> mountPointPath
                    pure False
                  Just tableFsPath -> do
                    let loadTableByHardlink = do
                          LSMT.importSnapshot session snapshotName tableFsPath
                          pure True
                    -- NOTE: @base@ does not expose EXDEV, so we fall back to exporting via
                    --       copy if we encounter _any_ IOException, not just EXDEV.
                    let handleIOException :: IOException -> IO Bool
                        handleIOException e = do
                          writeLog logger DEBUG . T.pack $
                            "Could not load table " <> tableName <> " by hardlinking from " <> tableAbsFilePath <> ": " <> displayException e
                          pure False
                    loadTableByHardlink `catch` handleIOException
              if success
                then
                  writeLog logger DEBUG . T.pack $
                    "Hardlink table " <> tableName <> " from " <> tableAbsFilePath <> " succeeded."
                else
                  writeLog logger DEBUG . T.pack $
                    "Hardlink table " <> tableName <> " from " <> tableAbsFilePath <> " failed..."
              pure success

            loadTableByCopy :: IO ()
            loadTableByCopy = do
              -- Copy to temporary directory, then import...
              let !sessionRootPath = FS.fsToFilePath mountPoint sessionRoot
              withTempDirectory sessionRootPath "active-imports" $ \importDir -> do
                !importAbsDir <- SD.makeAbsolute importDir
                let !importDirFsPath = fromJust (FS.fsFromFilePath mountPoint importAbsDir)
                writeLog logger DEBUG . T.pack $
                  "Trying to load table " <> tableName <> " by copying from " <> tableAbsFilePath <> " to " <> importAbsDir
                copyRecursive tableAbsFilePath importDir
                writeLog logger DEBUG . T.pack $
                  "Trying to load table " <> tableName <> " by hardlinking from " <> importAbsDir
                LSMT.importSnapshot session snapshotName importDirFsPath

        -- Load the snapshot.
        bracket_ loadSnapshot (deleteSnapshot session snapshotName) $
          -- Open the table from the snapshot.
          LSMT.withTableFromSnapshot session snapshotName snapshotLabel $ \table ->
            -- Run the action.
            action LSMTreeTable{..}

{- |
Save a table.

The target directory must not already exist.
-}
saveTable :: Logger IO -> Table k v -> FilePath -> IO ()
saveTable logger LSMTreeTable{..} targetDir = do
  -- Test if the target exists...
  targetDirExists <- SD.doesPathExist targetDir
  when targetDirExists . throwIO $ TargetExistsError targetDir
  -- Save a table snapshot...
  let !saveSnapshot = LSMT.saveSnapshot snapshotName snapshotLabel table
  bracket_ saveSnapshot (deleteSnapshot session snapshotName) $ do
    success <- trySaveTableByHardlink
    unless success saveTableByCopy
 where
  trySaveTableByHardlink :: IO Bool
  trySaveTableByHardlink =
    case FS.fsFromFilePath mountPoint targetDir of
      Nothing ->
        pure False
      Just targetDirFsPath -> do
        let saveTableByHardlink :: IO Bool
            saveTableByHardlink = do
              LSMT.exportSnapshot session snapshotName targetDirFsPath
              pure True
        -- NOTE: @base@ does not expose EXDEV, so we fall back to exporting via
        --       copy if we encounter _any_ IOException, not just EXDEV.
        let handleIOException :: IOException -> IO Bool
            handleIOException e = do
              writeLog logger WARN . T.pack $
                "Could not export table " <> tableName <> " by hardlink: " <> displayException e
              pure False
        saveTableByHardlink `catch` handleIOException

  saveTableByCopy :: IO ()
  saveTableByCopy = do
    -- Export to temporary directory, then copy...
    let !sessionRootPath = FS.fsToFilePath mountPoint sessionRoot
    withTempDirectory sessionRootPath "active-exports" $ \exportRootDir -> do
      let !exportDir = exportRootDir SF.</> tableName
      !exportAbsDir <- SD.makeAbsolute exportDir
      let !exportDirFsPath = fromJust (FS.fsFromFilePath mountPoint exportAbsDir)
      LSMT.exportSnapshot session snapshotName exportDirFsPath
      copyRecursive exportDir targetDir

{- |
Internal helper.

Delete an LSM Tree snapshot, but ignore any `LSMT.ErrSnapshotDoesNotExist` errors.
-}
deleteSnapshot :: LSMT.Session IO -> LSMT.SnapshotName -> IO ()
deleteSnapshot session snapshotName =
  LSMT.deleteSnapshot session snapshotName
    `catch` \LSMT.ErrSnapshotDoesNotExist{} -> pure ()

{- |
Internal helper.

Run an action with a new session.
-}
withNewSession ::
  Logger IO ->
  TableOptions k v ->
  (Session -> IO r) ->
  IO r
withNewSession logger LSMTreeTableOptions{..} action = do
  -- Create the snapshot name and label.
  let !snapshotName = LSMT.toSnapshotName tableName
  let !snapshotLabel = fromString tableLabel
  -- Create a temporary directory for the database session.
  let withSessionDir :: (FilePath -> IO a) -> IO a
      withSessionDir = case maybeSessionRoot of
        Nothing -> withSystemTempDirectory tableName
        Just sessionRoot -> withTempDirectory sessionRoot tableName
  withSessionDir $ \sessionRoot -> do
    writeLog logger DEBUG . T.pack $
      "Creating database session for " <> tableName <> " at " <> sessionRoot
    -- Create the LSM Tree session.
    !sessionAbsRoot <- SD.makeAbsolute sessionRoot
    let (!mountPointPath, !sessionRelRoot) = SF.splitDrive sessionAbsRoot
    let !mountPoint = FS.MountPoint mountPointPath
    let !sessionRelRootDirs = SF.splitDirectories sessionRelRoot
    let !sessionRootFsPath = FS.mkFsPath sessionRelRootDirs
    let !sessionDirFsPath = sessionRootFsPath FS.</> FS.mkFsPath ["session"]
    BIO.withIOHasBlockIO mountPoint BIO.defaultIOCtxParams $ \hasFS hasBlockIO -> do
      -- Create the session directory.
      FS.createDirectoryIfMissing hasFS True sessionDirFsPath
      -- Create the LSM Tree session.
      let sessionSalt = 0
      LSMT.withOpenSession mempty hasFS hasBlockIO sessionSalt sessionDirFsPath $ \session -> do
        writeLog logger DEBUG . T.pack $
          "Created database session for " <> tableName
        -- Run the action with the session.
        action LSMTreeSession{sessionRoot = sessionRootFsPath, ..}

{- |
Internal helper.

Copy a directory tree recursively.
-}
copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive source target = do
  sourceIsDirectory <- SD.doesDirectoryExist source
  if sourceIsDirectory
    then do
      -- If target exists, this throws the expected error.
      SD.createDirectory target
      entries <- SD.listDirectory source
      for_ entries $ \entry ->
        copyRecursive (source </> entry) (target </> entry)
    else do
      -- If source is a file, this succeeds.
      -- If source does not exist, this throws the expected error.
      SD.copyFile source target

{- |
Insert entries into a table.
-}
inserts ::
  Table k v -> Vector (k, v) -> IO ()
inserts = \case
  LSMTreeTable{..} ->
    LSMT.inserts table . fmap (\(k, v) -> (k, v, Nothing))

{- |
Lookup one entry from a table.
-}
lookup ::
  Table k v -> k -> IO (Maybe v)
lookup = \case
  LSMTreeTable{..} ->
    fmap LSMT.getValue . LSMT.lookup table

{- |
Lookup entries from a table.
-}
lookups ::
  Table k v -> Vector k -> IO (Vector (Maybe v))
lookups = \case
  LSMTreeTable{..} ->
    fmap (fmap LSMT.getValue) . LSMT.lookups table

{- |
Wrapper that derives the required `LSMT.SerialiseKey` and `LSMT.SerialiseValue`
instances from a `Binary` instance.
-}
newtype SerialiseViaBinary v = SerialiseViaBinary {value :: v}

instance (Binary v) => LSMT.SerialiseKey (SerialiseViaBinary v) where
  serialiseKey :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . B.encode . (.value)

  deserialiseKey :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseKey = SerialiseViaBinary . B.decode . LSMT.deserialiseKey

instance (Binary v) => LSMT.SerialiseValue (SerialiseViaBinary v) where
  serialiseValue :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . B.encode . (.value)

  deserialiseValue :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseValue = SerialiseViaBinary . B.decode . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseViaBinary v) instance LSMT.ResolveValue (SerialiseViaBinary v)

{- |
Wrapper that derives the required `LSMT.SerialiseKey` and `LSMT.SerialiseValue`
instances by unwrapping the newtype.
-}
newtype SerialiseVia v u = SerialiseVia {value :: v}

instance (Coercible v u, LSMT.SerialiseKey u) => LSMT.SerialiseKey (SerialiseVia v u) where
  serialiseKey :: SerialiseVia v u -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . coerce @_ @u

  deserialiseKey :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseKey = coerce @u @_ . LSMT.deserialiseKey

instance (Coercible v u, LSMT.SerialiseValue u) => LSMT.SerialiseValue (SerialiseVia v u) where
  serialiseValue :: SerialiseVia v u -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . coerce @_ @u

  deserialiseValue :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseValue = coerce @u @_ . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseVia v u) instance LSMT.ResolveValue (SerialiseVia v u)
