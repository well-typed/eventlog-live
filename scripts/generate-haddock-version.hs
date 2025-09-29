#!/usr/bin/env cabal
{- cabal:
build-depends:
    , base          >=4.16
    , bytestring    >=0.11
    , Cabal-syntax ^>=3.10 || ^>=3.12
-}
{-
This script reads the `version` field from a Cabal file.
-}

module Main (main) where

import qualified Data.ByteString as BS
import qualified Distribution.PackageDescription.Parsec as PackageDescription
import Distribution.Pretty (prettyShow)
import qualified Distribution.Types.GenericPackageDescription as GenericPackageDescription
import qualified Distribution.Types.PackageDescription as PackageDescription
import qualified Distribution.Types.PackageId as PackageId
import qualified Distribution.Types.Version as Version
import System.IO (hPutStrLn, stderr, stdin)

main :: IO ()
main = do
  cabalContent <- BS.hGetContents stdin
  case PackageDescription.parseGenericPackageDescriptionMaybe cabalContent of
    Nothing -> hPutStrLn stderr $ "error: Could not parse input"
    Just genericPackageDescription -> do
      let packageDescription = GenericPackageDescription.packageDescription genericPackageDescription
      let packageId = PackageDescription.package packageDescription
      let packageVersion = PackageId.pkgVersion packageId
      putStrLn (prettyShow packageVersion)
