module GHC.Eventlog.Live.Database (
  Session,
  SessionOptions (maybeSessionRoot),
  defaultSessionOptions,
  withNewSession,
  Table,
  TableOptions (..),
  withTable,
  saveTable,
  TableFormat (..),
  inferTableFormat,
  lookup,
  lookups,
  inserts,

  -- * Serialisation
  SerialiseViaBinary (..),
  SerialiseVia (..),

  -- * Errors
  TargetExistsError (..),
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (Exception (..), SomeException (..), bracket_, catch, throwIO)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.List qualified as L
import Data.Maybe (fromJust, fromMaybe)
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
import System.FilePath qualified as SF
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import Prelude hiding (lookup)

{- |
Representation of database sessions.
-}
type Session :: Type
data Session
  = LSMTreeSession
  { mountPoint :: FS.MountPoint
  , sessionRoot :: FS.FsPath
  , session :: LSMT.Session IO
  }

{- |
The options for database sessions.
-}
type SessionOptions :: Type
newtype SessionOptions
  = LSMTreeSessionOptions
  { maybeSessionRoot :: Maybe FilePath
  }

{- |
The default database session options.
-}
defaultSessionOptions :: SessionOptions
defaultSessionOptions =
  LSMTreeSessionOptions
    { maybeSessionRoot = Nothing
    }

{- |
Run an action with a new session.
-}
withNewSession ::
  Logger IO ->
  SessionOptions ->
  (Session -> IO r) ->
  IO r
withNewSession logger LSMTreeSessionOptions{..} action = do
  -- Create a temporary directory for the database session.
  let withSessionDir :: (FilePath -> IO a) -> IO a
      withSessionDir = case maybeSessionRoot of
        Nothing -> withSystemTempDirectory "eventlog-live"
        Just sessionRoot -> withTempDirectory sessionRoot "eventlog-live"
  withSessionDir $ \sessionRoot -> do
    writeLog logger DEBUG . T.pack $
      "Creating database session at " <> sessionRoot <> "."
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
      LSMT.withNewSession mempty hasFS hasBlockIO sessionSalt sessionDirFsPath $ \session -> do
        writeLog logger DEBUG . T.pack $
          "Created database session."
        -- Run the action with the session.
        action LSMTreeSession{sessionRoot = sessionRootFsPath, ..}

{- |
Representation of database tables.
-}
type Table :: Type -> Type -> Type
data Table k v
  = (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v) =>
  LSMTreeTable
  { session :: Session
  , table :: LSMT.Table IO k v Void
  , tableName :: String
  , snapshotName :: LSMT.SnapshotName
  , snapshotLabel :: LSMT.SnapshotLabel
  }

{- |
The options for database tables.
-}
type TableOptions :: Type -> Type -> Type
data TableOptions k v
  = (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v) => LSMTreeTableOptions
  { tableName :: String
  , tableLabel :: String
  , maybeTableFilePath :: Maybe FilePath
  }

newtype TargetExistsError = TargetExistsError FilePath
  deriving stock (Show)

instance Exception TargetExistsError

{- |
Run an action with a table.

If @`TableOptions`.`maybeTableFilePath`@ is set, the table is imported from the given snapshot.
-}
withTable ::
  Logger IO ->
  Session ->
  TableOptions k v ->
  (Table k v -> IO a) ->
  IO a
withTable logger session@LSMTreeSession{session = lsmtSession, ..} LSMTreeTableOptions{..} action = do
  let !snapshotName = LSMT.toSnapshotName tableName
  let !snapshotLabel = fromString tableLabel
  case maybeTableFilePath of
    Nothing ->
      -- Create a new LSM Tree table.
      LSMT.withTable lsmtSession $ \table ->
        -- Run the action.
        action LSMTreeTable{..}
    Just tableRelFilePath -> do
      -- Find the absolute file path to the table.
      tableAbsFilePath <- SD.makeAbsolute tableRelFilePath

      -- Load a table in the LSMTreeSnapshotV2 format.
      let loadLSMTreeSnapshotV2 :: IO ()
          loadLSMTreeSnapshotV2 = do
            -- Write a log message.
            writeLog logger DEBUG . T.pack $
              "Import table " <> tableName <> " from " <> tableRelFilePath <> " by hard linking."
            -- Try to represent the target directory as an FsPath.
            let FS.MountPoint mountPointPath = mountPoint
            let !targetFsPath =
                  fromMaybe (error $ "Cannot hardlink from " <> tableAbsFilePath <> "; not under mount point " <> mountPointPath <> ".") $
                    FS.fsFromFilePath mountPoint tableAbsFilePath
            -- Export the snapshot.
            let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [tableName]
            LSMT.importSnapshot lsmtSession snapshotName snapshotFsPath

      -- Load a table in the LSMTreeSnapshotV2Tar format.
      let loadLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
          loadLSMTreeSnapshotV2Tar decompress = do
            -- Write a log message.
            writeLog logger DEBUG . T.pack $
              "Import table " <> tableName <> " from " <> tableRelFilePath <> " by unarchiving."
            -- Create temporary @active-import@ directory in the database session root.
            let !sessionRootPath = FS.fsToFilePath mountPoint sessionRoot
            withTempDirectory sessionRootPath "active-import" $ \importDir -> do
              -- Extract the snapshot to @active-import/$tableName@.
              !importAbsDir <- SD.makeAbsolute importDir
              let !importDirFsPath = fromJust (FS.fsFromFilePath mountPoint importAbsDir)
              let !snapshotDirFsPath = importDirFsPath FS.</> FS.mkFsPath [tableName]
              tarByteString <- BSL.readFile tableAbsFilePath
              let tarEntries = Tar.read . decompress $ tarByteString
              let tarCheck entry = SomeException <$> Tar.checkEntrySecurity entry
              Tar.unpackAndCheck tarCheck importAbsDir tarEntries
              -- Import the snapshot from @active-import/$tableName@.
              LSMT.importSnapshot lsmtSession snapshotName snapshotDirFsPath

      -- Load a table based on the inferred table format.
      let loadSnapshot :: IO ()
          loadSnapshot =
            case inferTableFormat tableAbsFilePath of
              LSMTreeSnapshotV2 -> loadLSMTreeSnapshotV2
              LSMTreeSnapshotV2Tar -> loadLSMTreeSnapshotV2Tar id
              LSMTreeSnapshotV2TarGz -> loadLSMTreeSnapshotV2Tar GZip.decompress

      -- Load the snapshot.
      bracket_ loadSnapshot (deleteSnapshot lsmtSession snapshotName) $
        -- Open the table from the snapshot.
        LSMT.withTableFromSnapshot lsmtSession snapshotName snapshotLabel $ \table ->
          -- Run the action.
          action LSMTreeTable{..}

{- |
An enumeration of table formats.
-}
data TableFormat
  = -- | The table is exported as a directory that contains an `lsm-tree` snapshot.
    LSMTreeSnapshotV2
  | -- | The table is exported as the tar archive of an `LSMTreeSnapshotV2` export.
    LSMTreeSnapshotV2Tar
  | -- | The table is exported as the GZip-compressed tar archive of an `LSMTreeSnapshotV2` export.
    LSMTreeSnapshotV2TarGz

{- |
Infer the table format from a filename.

[If the filename matches @*.lsm2.d@]:
  The format is `LSMTreeSnapshotV2`.
[If the filename matches @*.lsm2@]:
  The format is `LSMTreeSnapshotV2Tar`.
[If the filename matches @*.lsm2.gz@]:
  The format is `LSMTreeSnapshotV2TarGz`.
[Otherwise]:
  The format defaults to `LSMTreeSnapshotV2Tar`.
-}
inferTableFormat :: FilePath -> TableFormat
inferTableFormat filePath
  | ".lsm2.d" `L.isSuffixOf` filePath = LSMTreeSnapshotV2
  | ".lsm2" `L.isSuffixOf` filePath = LSMTreeSnapshotV2Tar
  | ".lsm2.gz" `L.isSuffixOf` filePath = LSMTreeSnapshotV2TarGz
  | otherwise = LSMTreeSnapshotV2Tar

{- |
Save a table.

The target directory must not already exist.
-}
saveTable :: Logger IO -> Table k v -> FilePath -> IO ()
saveTable logger LSMTreeTable{session = LSMTreeSession{..}, ..} target = do
  -- Test if the target exists...
  targetExists <- SD.doesPathExist target
  when targetExists . throwIO $ TargetExistsError target

  -- Save a table snapshot...
  let !saveSnapshot = LSMT.saveSnapshot snapshotName snapshotLabel table
  bracket_ saveSnapshot (deleteSnapshot session snapshotName) $
    case inferTableFormat target of
      LSMTreeSnapshotV2 -> saveLSMTreeSnapshotV2
      LSMTreeSnapshotV2Tar -> saveLSMTreeSnapshotV2Tar id
      LSMTreeSnapshotV2TarGz -> saveLSMTreeSnapshotV2Tar GZip.compress
 where
  saveLSMTreeSnapshotV2 :: IO ()
  saveLSMTreeSnapshotV2 = do
    -- Write a log message.
    writeLog logger DEBUG . T.pack $
      "Export table " <> tableName <> " to " <> target <> " by hard linking."
    -- Try to represent the target directory as an FsPath.
    let FS.MountPoint mountPointPath = mountPoint
    absTarget <- SD.makeAbsolute target
    let !targetFsPath =
          fromMaybe (error $ "Cannot hardlink to " <> target <> "; not under mount point " <> mountPointPath <> ".") $
            FS.fsFromFilePath mountPoint absTarget

    -- Create the target directory.
    SD.createDirectory target
    -- Export the snapshot.
    let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [tableName]
    LSMT.exportSnapshot session snapshotName snapshotFsPath

  saveLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
  saveLSMTreeSnapshotV2Tar compress = do
    -- Write a log message.
    writeLog logger DEBUG . T.pack $
      "Export table " <> tableName <> " to " <> target <> " by archiving."
    -- Create temporary @active-export@ directory in the database session root.
    let !sessionRootPath = FS.fsToFilePath mountPoint sessionRoot
    withTempDirectory sessionRootPath "active-export" $ \exportRootDir -> do
      -- Export the snapshot to the temporary @active-export@ directory.
      let !snapshotDir = exportRootDir SF.</> tableName
      !snapshotAbsDir <- SD.makeAbsolute snapshotDir
      let !snapshotDirFsPath = fromJust (FS.fsFromFilePath mountPoint snapshotAbsDir)
      LSMT.exportSnapshot session snapshotName snapshotDirFsPath
      -- Create the output tar archive.
      BSL.writeFile target . compress =<< Tar.write' =<< Tar.pack' exportRootDir [tableName]

{- |
Internal helper.

Delete an LSM Tree snapshot, but ignore any `LSMT.ErrSnapshotDoesNotExist` errors.
-}
deleteSnapshot :: LSMT.Session IO -> LSMT.SnapshotName -> IO ()
deleteSnapshot session snapshotName =
  LSMT.deleteSnapshot session snapshotName
    `catch` \LSMT.ErrSnapshotDoesNotExist{} -> pure ()

-- entries <- SD.listDirectory base
-- for entries $ \entry ->
--   entry

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

Derives `LSMT.ResolveValue` via `LSMT.ResolveAsFirst`.
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

Derives `LSMT.ResolveValue` via `LSMT.ResolveAsFirst`.
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
