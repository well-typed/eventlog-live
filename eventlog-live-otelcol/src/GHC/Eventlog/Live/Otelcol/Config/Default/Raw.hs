{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config.Default.Raw
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Default.Raw (
  defaultConfigByteString,
  defaultConfigString,
  defaultConfigJSONSchemaByteString,
  defaultConfigJSONSchemaString,

  -- * Compile-time only helper functions
  decodeThrow,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed (embedFileRelative)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.YAML (FromYAML)
import Data.YAML qualified as YAML

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
decodeThrow :: (MonadFail m, FromYAML a) => ByteString -> m a
decodeThrow byteString =
  either (fail . prettyErrorMessage) pure $ YAML.decode1Strict byteString
 where
  prettyErrorMessage :: (YAML.Pos, String) -> String
  prettyErrorMessage (pos, errorMessage) =
    YAML.prettyPosWithSource pos (BSL.fromStrict byteString) " error" <> errorMessage

{- |
Internal helper.
Decode a `ByteString` to a `String`.
-}
fromByteString :: ByteString -> String
fromByteString = T.unpack . TE.decodeUtf8Lenient
