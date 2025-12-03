{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Index (
  defaultMain,
  InfoProvDb (..),
  withInfoProvDb,
  generateInfoProvDb,
  lookupInfoProv,
  InfoProv (..),
  IpeId (..),
)
where

import Data.Foldable (traverse_)
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only (..))
import GHC.Eventlog.InfoProv as Ipe
import GHC.Eventlog.Table as Table
import GHC.RTS.Events (
  Data (..),
  Event (..),
  EventLog (EventLog),
  readEventLogFromFile,
 )
import qualified GHC.RTS.Events as Ghc
import qualified GHC.Eventlog.Options as Opts
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

defaultMain :: IO ()
defaultMain = do
  opts <- Opts.parseOptions
  case opts.command of
    Opts.Query query -> withInfoProvDb opts.databaseFp $ \ db -> do
      lookupInfoProv db query.ipeId >>= \ case
        Nothing -> Text.putStrLn $ "No Info Prov found for " <> Text.show query.ipeId
        Just prov -> Text.putStrLn $ prettyInfoProv prov
    Opts.Index index -> withInfoProvDb opts.databaseFp $ \ db -> do
      generateInfoProvDb db index.eventlog

prettyInfoProv :: InfoProv -> Text
prettyInfoProv prov =
  Text.unlines
    [ "Info Table  "        <> prettyIpeId prov.infoId
    , "  Table Name:      " <> prov.tableName
    , "  Closure Desc:    " <> Text.show prov.closureDesc
    , "  Type Desc:       " <> prov.typeDesc
    , "  Label:           " <> prov.label
    , "  Modulename:      " <> prov.moduleName
    , "  Source Location: " <> prov.srcLoc
    ]

-- ----------------------------------------------------------------------------
-- Db type
-- ----------------------------------------------------------------------------

data InfoProvDb = InfoProvDb
  { conn :: Sqlite.Connection
  }

-- ----------------------------------------------------------------------------
-- High Level API
-- ----------------------------------------------------------------------------

withInfoProvDb :: FilePath -> (InfoProvDb -> IO a) -> IO a
withInfoProvDb fp act = Sqlite.withConnection fp $ \conn ->
  act (InfoProvDb{conn})

generateInfoProvDb :: InfoProvDb -> FilePath -> IO ()
generateInfoProvDb db fp = do
  Sqlite.withExclusiveTransaction db.conn $ setupDb db.conn
  setupIndexing db.conn
  readEventLogFromFile fp >>= \case
    Left err -> fail err
    Right (EventLog _h (Data es)) -> Sqlite.withExclusiveTransaction db.conn $ do
      traverse_ (processIpeEvents db.conn) es

lookupInfoProv :: InfoProvDb -> IpeId -> IO (Maybe InfoProv)
lookupInfoProv db ipeId = do
  r <- Sqlite.query db.conn findInfoTableQuery (Only ipeId)
  case r of
    [ipe] -> pure $ Just ipe
    _ -> pure $ Nothing

-- ----------------------------------------------------------------------------
-- Low Level Sqlite api
-- ----------------------------------------------------------------------------

setupDb :: Sqlite.Connection -> IO ()
setupDb conn = do
  Sqlite.execute_ conn dropStringTableStmt
  Sqlite.execute_ conn dropInfoProvTableStmt
  Sqlite.execute_ conn dropInfoProvTableViewStmt
  Sqlite.execute_ conn stringTableStmt
  Sqlite.execute_ conn infoProvTableStmt
  Sqlite.execute_ conn infoProvTableViewStmt

setupIndexing :: Sqlite.Connection -> IO ()
setupIndexing conn = do
  Sqlite.execute_ conn "PRAGMA synchronous = OFF;"
  Sqlite.execute_ conn "PRAGMA journal_mode = OFF;"
  Sqlite.execute_ conn "PRAGMA temp_store = MEMORY;"
  Sqlite.execute_ conn "PRAGMA locking_mode = EXCLUSIVE;"

insertInfoProv :: Sqlite.Connection -> InfoProv -> IO ()
insertInfoProv conn prov = do
  row <- upsertInfoProvStrings conn prov
  _ <- Sqlite.execute conn insertInfoTableQuery row
  pure ()

upsertInfoProvStrings :: Sqlite.Connection -> InfoProv -> IO InfoProvRow
upsertInfoProvStrings conn prov = do
  Sqlite.executeMany
    conn
    insertOrIgnoreString
    [ Only prov.tableName
    , Only prov.typeDesc
    , Only prov.label
    , Only prov.moduleName
    , Only prov.srcLoc
    ]
  [(taId, tyId, labelId, modId, srcLocId)] <-
    Sqlite.query
      conn
      getIpeStrings
      ( prov.typeDesc
      , prov.label
      , prov.moduleName
      , prov.srcLoc
      , prov.tableName
      )
  pure $
    InfoProvRow
      { Table.infoId = prov.infoId
      , Table.tableName = taId
      , Table.closureDesc = prov.closureDesc
      , Table.typeDesc = tyId
      , Table.label = labelId
      , Table.moduleName = modId
      , Table.srcLoc = srcLocId
      }

-- ----------------------------------------------------------------------------
-- Eventlog processing
-- ----------------------------------------------------------------------------

processIpeEvents :: Sqlite.Connection -> Event -> IO ()
processIpeEvents conn ev = case eventInfoToInfoProv (evSpec ev) of
  Nothing -> pure ()
  Just infoProv -> insertInfoProv conn infoProv

eventInfoToInfoProv :: Ghc.EventInfo -> Maybe InfoProv
eventInfoToInfoProv ev = case ev of
  it@Ghc.InfoTableProv{} ->
    Just
      InfoProv
        { Ipe.infoId = IpeId $ Ghc.itInfo it
        , Ipe.tableName = Ghc.itTableName it
        , Ipe.closureDesc = fromIntegral $ Ghc.itClosureDesc it
        , Ipe.typeDesc = Ghc.itTyDesc it
        , Ipe.label = Ghc.itLabel it
        , Ipe.moduleName = Ghc.itModule it
        , Ipe.srcLoc = Ghc.itSrcLoc it
        }
  _ -> Nothing
