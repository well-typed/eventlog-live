#!/usr/bin/env cabal
{- cabal:
build-depends:
    , base          >=4.16
    , bytestring    >=0.11
    , Cabal-syntax ^>=3.10  || ^>=3.12
    , text          >=2.1
-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Distribution.PackageDescription.Parsec as PackageDescription
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.PackageDescription as PackageDescription
import Distribution.Utils.ShortText (fromShortText)
import System.IO (hPutStrLn, stderr, stdin, stdout)

main :: IO ()
main = do
  cabalContent <- BS.hGetContents stdin
  case PackageDescription.parseGenericPackageDescriptionMaybe cabalContent of
    Nothing -> hPutStrLn stderr $ "error: Could not parse input"
    Just genericPackageDescription -> do
      let packageDescription = GenericPackageDescription.packageDescription genericPackageDescription
      let description = T.pack . fromShortText $ PackageDescription.description packageDescription
      TIO.putStrLn description
