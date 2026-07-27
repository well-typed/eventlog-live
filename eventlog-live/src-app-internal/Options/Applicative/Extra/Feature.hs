{-# LANGUAGE OverloadedStrings #-}

module Options.Applicative.Extra.Feature (
  Feature (..),
  helpFor,
  onlyFor,
  exitIfUnsupported,
) where

import Control.Monad (unless)
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty qualified as OP
import System.Exit (exitFailure)

data Feature = Feature
  { flag :: !String
  , isOn :: !Bool
  , info :: !String
  }

{- |
Create a command-line help document for an option that depend on a feature flag.
-}
helpFor :: Feature -> String -> O.Mod f a
helpFor feature help
  | feature.isOn = O.help help
  | otherwise = O.helpDoc (Just $ OP.vcat [OP.pretty unsupported, OP.pretty help]) <> O.hidden
 where
  unsupported :: Text
  unsupported = T.pack "Unsupported. Requires build with -f+" <> T.pack feature.flag <> "."

{- |
Create a command-line option that depends on a feature flag.
-}
onlyFor ::
  forall f a.
  (O.HasName f, Default a) =>
  Feature ->
  (O.Mod f a -> O.Parser a) ->
  O.Mod f a ->
  (forall g x. (O.HasName g) => O.Mod g x) ->
  O.Parser a
onlyFor feature opt optIfSupported optAlways
  | feature.isOn = opt (optAlways <> optIfSupported)
  | otherwise = def <$ O.infoOption unsupportedInfo optAlways
 where
  unsupportedInfo :: String
  unsupportedInfo = feature.info <> " Requires build with -f+" <> feature.flag <> "."

{- |
Exit with the feature info.
-}
exitIfUnsupported :: Feature -> Logger IO -> IO ()
exitIfUnsupported feature logger =
  unless feature.isOn $ do
    writeLog logger FATAL (T.pack feature.info)
    exitFailure
