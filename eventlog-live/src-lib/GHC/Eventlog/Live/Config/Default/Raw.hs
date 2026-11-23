{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GHC.Eventlog.Live.Config.Default.Raw
Description : The implementation of @eventlog-live@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Config.Default.Raw (
  defaultConfigByteString,
  defaultConfigString,
  defaultConfigJSONSchemaByteString,
  defaultConfigJSONSchemaString,

  -- * Compile-time only helper functions
  decodeDefaultConfigOrThrow,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Yamlet (FromYaml)
import Yamlet qualified as Yaml

{- |
Internal helper.
The relative path to the default configuration file.
-}
defaultConfigFilePath :: FilePath
defaultConfigFilePath = "data/default.yaml"

{- |
Internal helper.
The default configuration as a `ByteString`.
-}
defaultConfigByteString :: ByteString
defaultConfigByteString = $(embedFileRelative "data/default.yaml")

{- |
Internal helper.
The default configuration as a `String`.
-}
defaultConfigString :: String
defaultConfigString = fromByteString defaultConfigByteString

{- |
Internal helper.
The default configuration as a `ByteString`.
-}
defaultConfigJSONSchemaByteString :: ByteString
defaultConfigJSONSchemaByteString = $(embedFileRelative "data/config.schema.json")

{- |
Internal helper.
The default configuration as a `String`.
-}
defaultConfigJSONSchemaString :: String
defaultConfigJSONSchemaString = fromByteString defaultConfigJSONSchemaByteString

{- |
__Warning:__ Compile-time only.

Internal helper.
Decode a `ByteString` or throw an exception.
-}
decodeDefaultConfigOrThrow :: (MonadFail m, FromYaml a) => ByteString -> m a
decodeDefaultConfigOrThrow byteString =
  either (fail . prettyErrorMessage) pure $ Yaml.decode byteString
 where
  prettyErrorMessage :: NonEmpty Yaml.Error -> String
  prettyErrorMessage = unlines . fmap (Yaml.prettyError defaultConfigFilePath) . NonEmpty.toList

{- |
Internal helper.
Decode a `ByteString` to a `String`.
-}
fromByteString :: ByteString -> String
fromByteString = T.unpack . TE.decodeUtf8Lenient
