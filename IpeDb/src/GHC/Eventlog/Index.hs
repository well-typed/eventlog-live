{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Eventlog.Split (
  generateIpeIndex,
  listIds,
  getIndexEntry,
  searchBatch,
  search,
  IpeId (..),
)
where

import Data.Binary
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Int
import qualified Data.List as List
import Data.Machine
import Data.Machine.Runner
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Traversable (forM)
import Data.Tuple (Solo (..))
import qualified Data.Vector as V
import qualified GHC.IO.Handle as LBS
import GHC.RTS.Events (
  Data (..),
  Event (..),
  EventLog (EventLog),
  readEventLogFromFile,
 )
import qualified GHC.RTS.Events as Ghc
import qualified Numeric
import System.FilePath
import System.IO

generateIpeIndex :: FilePath -> IO ()
generateIpeIndex fp = do
  readEventLogFromFile fp >>= \case
    Left err -> fail err
    Right (EventLog _h (Data es)) -> do
      let Identity ipeMap =
            foldlT processIpeEvents emptyIpeMap $
              source es

      sinkToFile (fp <.> "ipe") ipeMap

emptyIpeMap :: IpeMap
emptyIpeMap =
  IpeMap
    { infoProvs = Map.empty
    }

processIpeEvents :: IpeMap -> Event -> IpeMap
processIpeEvents ipeMap ev = case eventInfoToInfoProv (evSpec ev) of
  Nothing -> ipeMap
  Just infoProv -> addIpeEntry ipeMap infoProv

eventInfoToInfoProv :: Ghc.EventInfo -> Maybe InfoProv
eventInfoToInfoProv ev = case ev of
  it@Ghc.InfoTableProv{} ->
    Just
      InfoProv
        { infoId = IpeId $ Ghc.itInfo it
        , tableName = Ghc.itTableName it
        , closureDesc = Ghc.itClosureDesc it
        , typeDesc = Ghc.itTyDesc it
        , label = Ghc.itLabel it
        , moduleName = Ghc.itModule it
        , srcLoc = Ghc.itSrcLoc it
        }
  _ -> Nothing

addIpeEntry :: IpeMap -> InfoProv -> IpeMap
addIpeEntry ipeMap ev =
  ipeMap
    { infoProvs = Map.insert ev.infoId ev ipeMap.infoProvs
    }

data InfoProv = InfoProv
  { infoId :: !IpeId
  , tableName :: !Text
  , closureDesc :: !Int
  , typeDesc :: !Text
  , label :: !Text
  , moduleName :: !Text
  , srcLoc :: !Text
  }
  deriving (Show, Eq, Ord)

newtype IpeId = IpeId {id :: Word64}
  deriving (Eq, Ord)

instance Show IpeId where
  show ipeId = "IpeId {id = " <> showAsHex (ipeId.id) <> "}"

showAsHex :: (Integral a) => a -> String
showAsHex n = "0x" <> Numeric.showHex n ""

data IpeMap = IpeMap
  { infoProvs :: !(Map IpeId InfoProv)
  }
  deriving (Show, Eq, Ord)

instance Binary IpeId where
  put ipeId = Put.putWord64be ipeId.id
  get = IpeId <$> Get.getWord64be

instance Binary InfoProv where
  put ipe = do
    put ipe.infoId
    put ipe.tableName
    put ipe.closureDesc
    put ipe.typeDesc
    put ipe.label
    put ipe.moduleName
    put ipe.srcLoc

  get =
    InfoProv
      <$> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get
      <*> get

-- ----------------------------------------------------------------------------
-- Lookups
-- ----------------------------------------------------------------------------

indexConstant :: LBS.ByteString
indexConstant = "IPE\0IPE\0"

indexConstantSize :: Int
indexConstantSize = fromIntegral @Int64 @Int $ LBS.length indexConstant

listIndex :: FilePath -> IO [IndexEntry]
listIndex fp = withBinaryFile fp ReadMode $ \h -> do
  meta <- getMetadataFromHandle h
  index <- getFileIndexFromHandle h meta
  listFileIndex index

listIds :: FilePath -> IO [IpeId]
listIds fp = do
  fmap (.ipeId) <$> listIndex fp

getIndexEntry :: FilePath -> IpeId -> IO IndexEntry
getIndexEntry fp idx = withBinaryFile fp ReadMode $ \h -> do
  meta <- getMetadataFromHandle h
  index <- getFileIndexFromHandle h meta
  lookupIndexEntry index idx

search :: FilePath -> IpeId -> IO InfoProv
search fp i = do
  MkSolo ipe <- searchBatch fp (MkSolo i)
  pure ipe

searchBatch :: (Traversable t) => FilePath -> t IpeId -> IO (t InfoProv)
searchBatch fp indices = withBinaryFile fp ReadMode $ \h -> do
  metadata <- getMetadataFromHandle h
  index <- getFileIndexFromHandle h metadata
  forM indices $ \idx -> do
    ixEntry <- lookupIndexEntry index idx
    e <- getEntryAt h metadata ixEntry
    pure $ Get.runGet get e.payload

-- ----------------------------------------------------------------------------
-- FileIndex
-- ----------------------------------------------------------------------------

data FileIndex = FileIndex
  { entries :: Map IpeId IndexEntry
  , handle :: Handle
  }

listFileIndex :: FileIndex -> IO [IndexEntry]
listFileIndex findex = pure $ fmap snd $ Map.toAscList findex.entries

lookupIndexEntry :: FileIndex -> IpeId -> IO IndexEntry
lookupIndexEntry findex ipeId = pure $ findex.entries Map.! ipeId

getFileIndexFromHandle :: Handle -> Metadata -> IO FileIndex
getFileIndexFromHandle h meta = do
  LBS.hSeek h AbsoluteSeek sizeOfMetadata
  indexLbs <- LBS.hGet h (sizeOfIndex meta)
  let
    idx = parseIndexTable indexLbs
  pure $ indexToLookupTable idx
 where
  parseIndexTable lbs = Get.runGet (get @Index) lbs

  indexToLookupTable :: Index -> FileIndex
  indexToLookupTable idx =
    FileIndex
      { entries =
          Map.fromList
            $ fmap
              ( \i ->
                  ( i.ipeId
                  , IndexEntry
                      { ipeId = i.ipeId
                      , offset = fromIntegral i.offset
                      , size = fromIntegral i.size
                      }
                  )
              )
            $ V.toList idx.indices
      , handle = h
      }

-- ----------------------------------------------------------------------------
-- Metadata
-- ----------------------------------------------------------------------------

data Metadata = Metadata
  { indexSize :: !Word64
  , dataStart :: !Word64
  -- ^ In Bytes
  }

mkMetadata :: Index -> Metadata
mkMetadata idx =
  Metadata
    { indexSize = fromIntegral $ V.length idx.indices
    , dataStart = 8 + fromIntegral (V.length idx.indices) * 3 * 8
    }

sizeOfIndex :: (Integral a) => Metadata -> a
sizeOfIndex meta = 8 + fromIntegral meta.indexSize * 3 * 8

sizeOfMetadata :: (Integral a) => a
sizeOfMetadata = fromIntegral indexConstantSize + 2 * 8

-- ----------------------------------------------------------------------------
-- Partial decoding
-- ----------------------------------------------------------------------------

getMetadataFromHandle :: Handle -> IO Metadata
getMetadataFromHandle h = do
  metadataLbs <- LBS.hGet h (indexConstantSize + 2 * 8)
  pure $ Get.runGet getMetadata metadataLbs

getMetadata :: Get Metadata
getMetadata = do
  _ <- Get.getByteString indexConstantSize
  size <- Get.getWord64be
  dataStart <- Get.getWord64be
  pure $
    Metadata
      { indexSize = size
      , dataStart = dataStart
      }

getEntryAt :: Handle -> Metadata -> IndexEntry -> IO Entry
getEntryAt h meta ixEntry = do
  hSeek
    h
    AbsoluteSeek
    (getAbsoluteDataStartSection meta + fromIntegral ixEntry.offset)
  bs <- LBS.hGet h $ fromIntegral ixEntry.size
  pure $ Get.runGet (Entry <$> Get.getLazyByteString (fromIntegral ixEntry.size)) bs

getAbsoluteDataStartSection :: Metadata -> Integer
getAbsoluteDataStartSection metadata =
  sizeOfMetadata + sizeOfIndex metadata

putMetadata :: Metadata -> Put
putMetadata m = do
  Put.putLazyByteString indexConstant
  Put.putWord64be m.indexSize
  Put.putWord64be m.dataStart

data Entry = Entry
  { payload :: LBS.ByteString
  }
  deriving (Show, Eq, Ord)

data Index = Index
  { indices :: V.Vector IndexEntry
  }
  deriving (Show, Eq, Ord)

data IndexEntry = IndexEntry
  { ipeId :: !IpeId
  , offset :: !Word64
  -- ^ Offset within the data region
  , size :: !Word64
  }
  deriving (Show, Eq, Ord)

sinkToFile :: FilePath -> IpeMap -> IO ()
sinkToFile fp ipeMap = do
  let
    (_offset, indices, entries) = List.foldl' go (0, [], []) (Map.toAscList ipeMap.infoProvs)
    index =
      Index
        { indices = V.fromList $ reverse indices
        }

    fullIndex = Put.runPut $ do
      putMetadata $ mkMetadata index
      put index
      traverse_ (Put.putLazyByteString . (.payload)) $ reverse entries

  LBS.writeFile fp fullIndex
 where
  go (!offset, indices, payloads) (ipeId, ipe) =
    let
      payload = Put.runPut (put ipe)
      entry =
        Entry
          { payload = payload
          }
      index =
        IndexEntry
          { ipeId
          , offset
          , size = fromIntegral (LBS.length payload)
          }
     in
      (offset + index.size, index : indices, entry : payloads)

instance Binary IndexEntry where
  put idx = do
    put idx.ipeId
    put idx.offset
    put idx.size

  get =
    IndexEntry
      <$> get
      <*> get
      <*> get

instance Binary Index where
  put idx = do
    put $ V.toList idx.indices

  get =
    Index
      <$> (V.fromList <$> get)
