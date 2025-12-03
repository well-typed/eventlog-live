{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GHC.Eventlog.Index (
  generateIpeIndex,

  InfoProv(..),
  IpeId (..),
)
where

import Data.Foldable (traverse_)
import GHC.RTS.Events (
  Data (..),
  Event (..),
  EventLog (EventLog),
  readEventLogFromFile,
 )
import qualified GHC.RTS.Events as Ghc
import System.FilePath
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only(..))
import GHC.Eventlog.InfoProv as Ipe
import GHC.Eventlog.Table as Table
import qualified Data.Text as Text
import Debug.Trace

setupDb :: Sqlite.Connection -> IO ()
setupDb conn = do
  Sqlite.execute_ conn dropStringTableStmt
  Sqlite.execute_ conn dropInfoProvTableStmt
  Sqlite.execute_ conn dropInfoProvTableViewStmt
  Sqlite.execute_ conn stringTableStmt
  Sqlite.execute_ conn infoProvTableStmt
  Sqlite.execute_ conn infoProvTableViewStmt

generateIpeIndex :: FilePath -> IO ()
generateIpeIndex fp = Sqlite.withConnection (fp <.> "sqlite") $ \ conn -> do
  Sqlite.setTrace conn (Just $ traceM . Text.unpack)
  setupDb conn
  readEventLogFromFile fp >>= \case
    Left err -> fail err
    Right (EventLog _h (Data es)) -> do
      traverse_ (processIpeEvents conn) es

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

insertInfoProv :: Sqlite.Connection -> InfoProv -> IO ()
insertInfoProv conn prov = do
  row <- upsertInfoProvStrings conn prov
  _ <- Sqlite.execute conn insertInfoTableQuery row
  pure ()

upsertInfoProvStrings :: Sqlite.Connection -> InfoProv -> IO InfoProvRow
upsertInfoProvStrings conn prov = do
  Sqlite.executeMany conn insertOrIgnoreString
    [ Only prov.tableName
    , Only prov.typeDesc
    , Only prov.label
    , Only prov.moduleName
    , Only prov.srcLoc
    ]
  [(taId, tyId, labelId, modId, srcLocId)] <- Sqlite.query conn getIpeStrings
    ( prov.tableName
    , prov.typeDesc
    , prov.label
    , prov.moduleName
    , prov.srcLoc
    )
  pure $
    InfoProvRow
      { Table.infoId = prov.infoId
      , Table.tableName =taId
      , Table.closureDesc = prov.closureDesc
      , Table.typeDesc = tyId
      , Table.label = labelId
      , Table.moduleName = modId
      , Table.srcLoc = srcLocId
      }



