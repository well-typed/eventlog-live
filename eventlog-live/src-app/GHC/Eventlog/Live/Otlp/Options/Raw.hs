{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : GHC.Eventlog.Live.Otlp.Options.Raw
Description : The implementation of @eventlog-live-otlp@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otlp.Options.Raw (
  headerString,
  progDescString,
  footerString,
) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

{- |
Internal helper.

The help header as a `ByteString`.
-}
headerByteString :: ByteString
headerByteString = $(embedFileRelative "data/help.header.txt")

{- |
Internal helper.

The help header as a `String`.
-}
headerString :: String
headerString = fromByteString headerByteString

{- |
Internal helper.

The help program description as a `ByteString`.
-}
progDescByteString :: ByteString
progDescByteString = $(embedFileRelative "data/help.progDesc.txt")

{- |
Internal helper.

The help program description as a `String`.
-}
progDescString :: String
progDescString = fromByteString progDescByteString

{- |
Internal helper.

The help footer as a `ByteString`.
-}
footerByteString :: ByteString
footerByteString = $(embedFileRelative "data/help.footer.txt")

{- |
Internal helper.

The help footer as a `String`.
-}
footerString :: String
footerString = fromByteString footerByteString

{- |
Internal helper.
Decode a `ByteString` to a `String`.
-}
fromByteString :: ByteString -> String
fromByteString = T.unpack . TE.decodeUtf8Lenient
