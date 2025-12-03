module GHC.Eventlog.Options (
  Options (..),
  Command(..),
  IndexOptions(..),
  QueryOptions(..),
  parseOptions,
  options,
  commands,
  queryCommand,
  indexCommand,
) where

import GHC.Eventlog.InfoProv (IpeId (..))
import Options.Applicative

data Options = Options
  { databaseFp :: FilePath
  , command :: Command
  }
  deriving (Show, Eq, Ord)

data Command
  = Index IndexOptions
  | Query QueryOptions
  deriving (Show, Eq, Ord)

data IndexOptions = IndexOptions
  { eventlog :: FilePath
  }
  deriving (Show, Eq, Ord)

data QueryOptions = QueryOptions
  { ipeId :: IpeId
  }
  deriving (Show, Eq, Ord)

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Index for Info Provenance entries"
     <> header "ipedb - A database for info provenance entries" )

options :: Parser Options
options =
  Options
    <$> strOption
      ( long "db"
          <> metavar "FILENAME"
          <> help "Database location"
      )
    <*> commands

commands :: Parser Command
commands =
  hsubparser
    ( command "index" (info (Index <$> indexCommand) (progDesc "Add a file to the repository"))
        <> command "query" (info (Query <$> queryCommand) (progDesc "Add a file to the repository"))
    )

queryCommand :: Parser QueryOptions
queryCommand =
  QueryOptions
    <$> ( IpeId
            <$> option auto (long "ipe" <> short 'i' <> help "Find the info table provenance information for the given key" <> metavar "INT")
        )

indexCommand :: Parser IndexOptions
indexCommand =
  IndexOptions
    <$> strOption (long "eventlog" <> short 'f' <> help "Eventlog location to index" <> metavar "FILENAME")
