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
  defaultConfigText,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE (decodeUtf8Lenient)

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
defaultConfigString = T.unpack defaultConfigText

{- |
Internal helper.
The default configuration as a `Text`.
-}
defaultConfigText :: Text
defaultConfigText = TE.decodeUtf8Lenient defaultConfigByteString
