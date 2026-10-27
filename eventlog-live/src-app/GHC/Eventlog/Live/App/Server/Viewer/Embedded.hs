{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module GHC.Eventlog.Live.App.Server.Viewer.Embedded where

import Crypto.Hash.MD5 qualified as MD5 (hashlazy)
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64B
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed (makeRelativeToProject)
import Data.Text qualified as T
import Language.Haskell.TH.Lift.Compat (Lift (..))
import WaiAppStatic.Storage.Embedded (EmbeddableEntry (..))

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

appleTouchIconPngPath :: FilePath
appleTouchIconPngPath = $(lift =<< makeRelativeToProject "data/viewer/apple-touch-icon.png")

faviconIcoPath :: FilePath
faviconIcoPath = $(lift =<< makeRelativeToProject "data/viewer/favicon.ico")

icon192PngPath :: FilePath
icon192PngPath = $(lift =<< makeRelativeToProject "data/viewer/icon-192.png")

icon512PngPath :: FilePath
icon512PngPath = $(lift =<< makeRelativeToProject "data/viewer/icon-512.png")

iconMaskPngPath :: FilePath
iconMaskPngPath = $(lift =<< makeRelativeToProject "data/viewer/icon-mask.png")

iconSvgPath :: FilePath
iconSvgPath = $(lift =<< makeRelativeToProject "data/viewer/icon.svg")

indexCssPath :: FilePath
indexCssPath = $(lift =<< makeRelativeToProject "data/viewer/index.css")

indexHtmlPath :: FilePath
indexHtmlPath = $(lift =<< makeRelativeToProject "data/viewer/index.html")

indexJsPath :: FilePath
indexJsPath = $(lift =<< makeRelativeToProject "data/viewer/index.js")

manifestJsonPath :: FilePath
manifestJsonPath = $(lift =<< makeRelativeToProject "data/viewer/manifest.webmanifest")

--------------------------------------------------------------------------------
-- Embeddable Entries
--------------------------------------------------------------------------------

hash :: BSL.ByteString -> T.Text
hash = T.take 8 . B64.extractBase64 . B64B.encodeBase64 . MD5.hashlazy

mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded = do
  appleTouchIconPngContent <- BSL.readFile appleTouchIconPngPath
  let appleTouchIconPngEntry =
        EmbeddableEntry
          { eLocation = "apple-touch-icon.png"
          , eMimeType = "image/png"
          , eContent = Left (hash appleTouchIconPngContent, appleTouchIconPngContent)
          }
  faviconIcoContent <- BSL.readFile faviconIcoPath
  let faviconIcoEntry =
        EmbeddableEntry
          { eLocation = "favicon.ico"
          , eMimeType = "image/x-icon"
          , eContent = Left (hash faviconIcoContent, faviconIcoContent)
          }
  icon192PngContent <- BSL.readFile icon192PngPath
  let icon192PngEntry =
        EmbeddableEntry
          { eLocation = "icon-192.png"
          , eMimeType = "image/png"
          , eContent = Left (hash icon192PngContent, icon192PngContent)
          }
  icon512PngContent <- BSL.readFile icon512PngPath
  let icon512PngEntry =
        EmbeddableEntry
          { eLocation = "icon-512.png"
          , eMimeType = "image/png"
          , eContent = Left (hash icon512PngContent, icon512PngContent)
          }
  iconMaskPngContent <- BSL.readFile iconMaskPngPath
  let iconMaskPngEntry =
        EmbeddableEntry
          { eLocation = "icon-mask.png"
          , eMimeType = "image/png"
          , eContent = Left (hash iconMaskPngContent, iconMaskPngContent)
          }
  iconSvgContent <- BSL.readFile iconSvgPath
  let iconSvgEntry =
        EmbeddableEntry
          { eLocation = "icon.svg"
          , eMimeType = "image/svg+xml"
          , eContent = Left (hash iconSvgContent, iconSvgContent)
          }
  indexCssContent <- BSL.readFile indexCssPath
  let indexCssEntry =
        EmbeddableEntry
          { eLocation = "index.css"
          , eMimeType = "text/css"
          , eContent = Left (hash indexCssContent, indexCssContent)
          }
  indexHtmlContent <- BSL.readFile indexHtmlPath
  let indexHtmlEntry =
        EmbeddableEntry
          { eLocation = "index.html"
          , eMimeType = "text/html"
          , eContent = Left (hash indexHtmlContent, indexHtmlContent)
          }
  indexJsContent <- BSL.readFile indexJsPath
  let indexJsEntry =
        EmbeddableEntry
          { eLocation = "index.js"
          , eMimeType = "text/javascript"
          , eContent = Left (hash indexJsContent, indexJsContent)
          }
  manifestJsonContent <- BSL.readFile manifestJsonPath
  let manifestJsonEntry =
        EmbeddableEntry
          { eLocation = "manifest.webmanifest"
          , eMimeType = "application/manifest+json"
          , eContent = Left (hash manifestJsonContent, manifestJsonContent)
          }
  pure
    [ appleTouchIconPngEntry
    , faviconIcoEntry
    , icon192PngEntry
    , icon512PngEntry
    , iconMaskPngEntry
    , iconSvgEntry
    , indexCssEntry
    , indexHtmlEntry
    , indexJsEntry
    , manifestJsonEntry
    ]
