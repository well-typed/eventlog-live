{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config.Default.Raw
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Default.Raw (
  defaultConfigByteString,
  defaultConfigJSONSchemaByteString,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)

{- |
Internal helper.
The default configuration as a `ByteString`.
-}
defaultConfigJSONSchemaByteString :: ByteString
defaultConfigJSONSchemaByteString = $(embedFileRelative "data/config.schema.json")

{- |
Internal helper.
The default configuration as a `ByteString`.
-}
defaultConfigByteString :: ByteString
defaultConfigByteString = $(embedFileRelative "data/default.yaml")
