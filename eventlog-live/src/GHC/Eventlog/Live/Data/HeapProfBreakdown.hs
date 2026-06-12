{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Data.InfoProv
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.HeapProfBreakdown (
  HeapProfBreakdown (..),
  heapProfBreakdownEitherReader,
  heapProfBreakdownShow,
  findHeapProfBreakdown,
) where

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.RTS.Events (HeapProfBreakdown (..))

{- |
Parses the `HeapProfBreakdown` command-line arguments:

> heapProfBreakdownEitherReader "T" == Left HeapProfBreakdownClosureType
> heapProfBreakdownEitherReader "c" == Left HeapProfBreakdownCostCentre
> heapProfBreakdownEitherReader "m" == Left HeapProfBreakdownModule
> heapProfBreakdownEitherReader "d" == Left HeapProfBreakdownClosureDescr
> heapProfBreakdownEitherReader "y" == Left HeapProfBreakdownTypeDescr
> heapProfBreakdownEitherReader "e" == Left HeapProfBreakdownEra
> heapProfBreakdownEitherReader "r" == Left HeapProfBreakdownRetainer
> heapProfBreakdownEitherReader "b" == Left HeapProfBreakdownBiography
> heapProfBreakdownEitherReader "i" == Left HeapProfBreakdownInfoTable
-}
heapProfBreakdownEitherReader :: String -> Either String HeapProfBreakdown
heapProfBreakdownEitherReader =
  \case
    "T" -> Right HeapProfBreakdownClosureType
    "c" -> Right HeapProfBreakdownCostCentre
    "m" -> Right HeapProfBreakdownModule
    "d" -> Right HeapProfBreakdownClosureDescr
    "y" -> Right HeapProfBreakdownTypeDescr
    "e" -> Right HeapProfBreakdownEra
    "r" -> Right HeapProfBreakdownRetainer
    "b" -> Right HeapProfBreakdownBiography
    "i" -> Right HeapProfBreakdownInfoTable
    str -> Left $ "Unsupported heap profile breakdown -h" <> str

{- |
Shows a `HeapProfBreakdown` as its corresponding command-line flag:

> heapProfBreakdownShow HeapProfBreakdownClosureType == "-hT"
> heapProfBreakdownShow HeapProfBreakdownCostCentre == "-hc"
> heapProfBreakdownShow HeapProfBreakdownModule == "-hm"
> heapProfBreakdownShow HeapProfBreakdownClosureDescr == "-hd"
> heapProfBreakdownShow HeapProfBreakdownTypeDescr == "-hy"
> heapProfBreakdownShow HeapProfBreakdownEra == "-he"
> heapProfBreakdownShow HeapProfBreakdownRetainer == "-hr"
> heapProfBreakdownShow HeapProfBreakdownBiography == "-hb"
> heapProfBreakdownShow HeapProfBreakdownInfoTable == "-hi"
-}
heapProfBreakdownShow :: HeapProfBreakdown -> String
heapProfBreakdownShow =
  ("-h" <>) . \case
    HeapProfBreakdownClosureType -> "T"
    HeapProfBreakdownCostCentre -> "c"
    HeapProfBreakdownModule -> "m"
    HeapProfBreakdownClosureDescr -> "d"
    HeapProfBreakdownTypeDescr -> "y"
    HeapProfBreakdownEra -> "e"
    HeapProfBreakdownRetainer -> "r"
    HeapProfBreakdownBiography -> "b"
    HeapProfBreakdownInfoTable -> "i"

{- |
Determine the `HeapProfBreakdown` from the list of program arguments.

__Warning__: This scan is not fully correct. It merely scans for the presence
of arguments that, as a whole, parse with `heapProfBreakdownEitherReader`.
It does not handle @-with-rtsopts@ and does not restrict its search to those
arguments between @+RTS@ and @-RTS@ tags.
-}
findHeapProfBreakdown :: [Text] -> Maybe HeapProfBreakdown
findHeapProfBreakdown = listToMaybe . mapMaybe parseHeapProfBreakdown
 where
  parseHeapProfBreakdown :: Text -> Maybe HeapProfBreakdown
  parseHeapProfBreakdown arg
    | "-h" `T.isPrefixOf` arg =
        either (const Nothing) Just
          . heapProfBreakdownEitherReader
          . T.unpack
          . T.drop 2
          $ arg
    | otherwise = Nothing
