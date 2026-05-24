{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Helheim.Index
  ( ReferenceIndex (..),
    fraudScore,
    loadIndex,
    saveIndex,
    searchIndex,
  )
where

import Control.Monad (when)
import Data.Binary.Put
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable)
import Helheim.Features (EncodedVector, dimensionCount)
import Helheim.PackedVector
import Helheim.Types
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import System.IO (Handle, IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hGetBuf, hSeek, withBinaryFile)

data ReferenceIndex = ReferenceIndex
  { referenceCount :: !Int,
    referenceVectorWords :: !(VS.Vector Word64),
    referenceLabelWords :: !(VS.Vector Word64),
    referenceNodeCount :: !Int,
    referenceNodeMeta :: !(VS.Vector Int32),
    referenceNodeBoundWords :: !(VS.Vector Word64)
  }
  deriving stock (Eq, Show)

loadIndex :: FilePath -> IO ReferenceIndex
loadIndex path =
  withBinaryFile path ReadMode $ \handle -> do
    header <- BS.hGet handle headerSizeV2
    case parseHeader header of
      Left err -> fail ("invalid reference index: " <> err)
      Right layout -> do
        fileSize <- hFileSize handle
        when (fileSize /= fromIntegral (layoutExpectedBytes layout)) $
          fail "invalid reference index: unexpected file length"
        hSeek handle AbsoluteSeek (fromIntegral (layoutHeaderSize layout))
        copyIndex handle layout

saveIndex :: FilePath -> ReferenceIndex -> IO ()
saveIndex path =
  BL.writeFile path . runPut . putIndex

searchIndex :: ReferenceIndex -> EncodedVector -> FraudResponse
searchIndex index query =
  let score = fraudScore index query
   in FraudResponse
        { fraudResponseApproved = score < 0.6,
          fraudResponseScore = score
        }

fraudScore :: ReferenceIndex -> EncodedVector -> Double
fraudScore index query =
  let neighborCount = min 5 (referenceCount index)
      frauds =
        if referenceNodeCount index > 0
          then kdFrauds index query
          else flatFrauds index query
   in if neighborCount == 0
        then 0
        else fromIntegral frauds / fromIntegral neighborCount

flatFrauds :: ReferenceIndex -> EncodedVector -> Int
flatFrauds index query =
  bestFraudCount (go 0 emptyBestNeighbors)
  where
    go !i !best
      | i >= referenceCount index = best
      | otherwise =
          let d = squaredDistance index query i
              labelValue = labelAt (referenceLabelWords index) i
              !best' = insertBest d labelValue best
           in go (i + 1) best'

kdFrauds :: ReferenceIndex -> EncodedVector -> Int
kdFrauds index query =
  bestFraudCount (goNode 0 0 emptyBestNeighbors)
  where
    -- 'nodeLB' is this node's lower bound, already computed by the parent (the
    -- root starts at 0, a valid lower bound). Threading it avoids recomputing
    -- the bound at every entry guard. We only recurse into a child when its
    -- bound is strictly below the current worst, and only then is the bound
    -- exact (not clamped), so the threaded value is always exact.
    goNode !node !nodeLB !best
      | node < 0 || node >= referenceNodeCount index =
          best
      | nodeLB >= worstDistance best =
          best
      | nodeLeft index node < 0 =
          scanLeaf (nodeStart index node) (nodeCount index node) best
      | otherwise =
          let left = nodeLeft index node
              right = nodeRight index node
              cutoff = worstDistance best
              lbLeft = lowerBoundUnder index query left cutoff
              lbRight = lowerBoundUnder index query right cutoff
              (first, firstBound, second, secondBound) =
                if lbLeft <= lbRight
                  then (left, lbLeft, right, lbRight)
                  else (right, lbRight, left, lbLeft)
              !bestAfterFirst =
                if firstBound < worstDistance best
                  then goNode first firstBound best
                  else best
           in if secondBound < worstDistance bestAfterFirst
                then goNode second secondBound bestAfterFirst
                else bestAfterFirst

    scanLeaf !start !count !best =
      loop start (start + count) best

    loop !i !end !best
      | i >= end = best
      | otherwise =
          let d = squaredDistanceUnder index query i (worstDistance best)
              labelValue = labelAt (referenceLabelWords index) i
              !best' = insertBest d labelValue best
           in loop (i + 1) end best'

data BestNeighbors = BestNeighbors
  { distance1 :: {-# UNPACK #-} !Int64,
    label1 :: {-# UNPACK #-} !Int,
    distance2 :: {-# UNPACK #-} !Int64,
    label2 :: {-# UNPACK #-} !Int,
    distance3 :: {-# UNPACK #-} !Int64,
    label3 :: {-# UNPACK #-} !Int,
    distance4 :: {-# UNPACK #-} !Int64,
    label4 :: {-# UNPACK #-} !Int,
    distance5 :: {-# UNPACK #-} !Int64,
    label5 :: {-# UNPACK #-} !Int
  }

emptyBestNeighbors :: BestNeighbors
emptyBestNeighbors =
  BestNeighbors
    maxDistance
    0
    maxDistance
    0
    maxDistance
    0
    maxDistance
    0
    maxDistance
    0

bestFraudCount :: BestNeighbors -> Int
bestFraudCount BestNeighbors {label1 = l1, label2 = l2, label3 = l3, label4 = l4, label5 = l5} =
  l1 + l2 + l3 + l4 + l5
{-# INLINE bestFraudCount #-}

worstDistance :: BestNeighbors -> Int64
worstDistance BestNeighbors {distance5 = d5} =
  d5
{-# INLINE worstDistance #-}

insertBest ::
  Int64 ->
  Int ->
  BestNeighbors ->
  BestNeighbors
insertBest
  d
  labelValue
  best@BestNeighbors
    { distance1 = d1,
      label1 = l1,
      distance2 = d2,
      label2 = l2,
      distance3 = d3,
      label3 = l3,
      distance4 = d4,
      label4 = l4,
      distance5 = d5
    }
  | d < d1 =
      BestNeighbors d labelValue d1 l1 d2 l2 d3 l3 d4 l4
  | d < d2 =
      BestNeighbors d1 l1 d labelValue d2 l2 d3 l3 d4 l4
  | d < d3 =
      BestNeighbors d1 l1 d2 l2 d labelValue d3 l3 d4 l4
  | d < d4 =
      BestNeighbors d1 l1 d2 l2 d3 l3 d labelValue d4 l4
  | d < d5 =
      BestNeighbors d1 l1 d2 l2 d3 l3 d4 l4 d labelValue
  | otherwise = best
{-# INLINE insertBest #-}

-- Exact (no cutoff) distance, used by the flat fallback search.
squaredDistance :: ReferenceIndex -> EncodedVector -> Int -> Int64
squaredDistance index =
  packedSquaredDistance (referenceVectorWords index)

-- Cutoff-aware variants used on the KD hot path; exact with respect to the
-- top-5 / pruning decisions (see Helheim.PackedVector).
squaredDistanceUnder :: ReferenceIndex -> EncodedVector -> Int -> Int64 -> Int64
squaredDistanceUnder index =
  packedSquaredDistanceUnder (referenceVectorWords index)

lowerBoundUnder :: ReferenceIndex -> EncodedVector -> Int -> Int64 -> Int64
lowerBoundUnder index =
  packedLowerBoundUnder (referenceNodeBoundWords index)

putIndex :: ReferenceIndex -> Put
putIndex index = do
  putByteString magicV3
  putWord64le (fromIntegral (referenceCount index))
  putWord32le scaleMarker
  putWord32le (fromIntegral (referenceNodeCount index))
  putWord32le (fromIntegral dimensionCount)
  VS.mapM_ putWord64le (referenceVectorWords index)
  VS.mapM_ putWord64le (referenceLabelWords index)
  VS.mapM_ putInt32le (referenceNodeMeta index)
  VS.mapM_ putWord64le (referenceNodeBoundWords index)

data IndexLayout = IndexLayout
  { layoutFormat :: !IndexFormat,
    layoutHeaderSize :: !Int,
    layoutCount :: !Int,
    layoutVectorBytes :: !Int,
    layoutLabelBytes :: !Int,
    layoutNodeCount :: !Int,
    layoutMetaBytes :: !Int,
    layoutBoundsBytes :: !Int,
    layoutExpectedBytes :: !Int
  }

data IndexFormat = FlatV1 | KdV2 | KdV3
  deriving stock (Eq, Show)

parseHeader :: BS.ByteString -> Either String IndexLayout
parseHeader header
  | BS.take (BS.length magicV3) header == magicV3 = headerLayoutV3 header
  | BS.take (BS.length magicV2) header == magicV2 = headerLayoutV2 header
  | BS.take (BS.length magicV1) header == magicV1 = headerLayoutV1 header
  | otherwise = Left "bad magic"

headerLayoutV1 :: BS.ByteString -> Either String IndexLayout
headerLayoutV1 header
  | BS.length header < headerSizeV1 = Left "file too small"
  | readWord32LE header 16 /= scaleMarker = Left "unsupported scale"
  | otherwise =
      Right
        IndexLayout
          { layoutFormat = FlatV1,
            layoutHeaderSize = headerSizeV1,
            layoutCount = count,
            layoutVectorBytes = vectorBytes,
            layoutLabelBytes = labelBytes,
            layoutNodeCount = 0,
            layoutMetaBytes = 0,
            layoutBoundsBytes = 0,
            layoutExpectedBytes = headerSizeV1 + vectorBytes + labelBytes
          }
  where
    count = fromIntegral (readWord64LE header 8)
    vectorBytes = count * dimensionCount * int16Bytes
    labelBytes = count

headerLayoutV2 :: BS.ByteString -> Either String IndexLayout
headerLayoutV2 header
  | BS.length header /= headerSizeV2 = Left "file too small"
  | readWord32LE header 16 /= scaleMarker = Left "unsupported scale"
  | readWord32LE header 24 /= fromIntegral dimensionCount = Left "unsupported dimensions"
  | otherwise =
      Right
        IndexLayout
          { layoutFormat = KdV2,
            layoutHeaderSize = headerSizeV2,
            layoutCount = count,
            layoutVectorBytes = vectorBytes,
            layoutLabelBytes = labelBytes,
            layoutNodeCount = parsedNodeCount,
            layoutMetaBytes = metaBytes,
            layoutBoundsBytes = boundsBytes,
            layoutExpectedBytes = headerSizeV2 + vectorBytes + labelBytes + metaBytes + boundsBytes
          }
  where
    count = fromIntegral (readWord64LE header 8)
    parsedNodeCount = fromIntegral (readWord32LE header 20)
    vectorBytes = count * dimensionCount * int16Bytes
    labelBytes = count
    metaBytes = parsedNodeCount * nodeMetaFields * int32Bytes
    boundsBytes = parsedNodeCount * dimensionCount * 2 * int16Bytes

headerLayoutV3 :: BS.ByteString -> Either String IndexLayout
headerLayoutV3 header
  | BS.length header /= headerSizeV3 = Left "file too small"
  | readWord32LE header 16 /= scaleMarker = Left "unsupported scale"
  | readWord32LE header 24 /= fromIntegral dimensionCount = Left "unsupported dimensions"
  | otherwise =
      Right
        IndexLayout
          { layoutFormat = KdV3,
            layoutHeaderSize = headerSizeV3,
            layoutCount = count,
            layoutVectorBytes = vectorBytes,
            layoutLabelBytes = labelBytes,
            layoutNodeCount = parsedNodeCount,
            layoutMetaBytes = metaBytes,
            layoutBoundsBytes = boundsBytes,
            layoutExpectedBytes = headerSizeV3 + vectorBytes + labelBytes + metaBytes + boundsBytes
          }
  where
    count = fromIntegral (readWord64LE header 8)
    parsedNodeCount = fromIntegral (readWord32LE header 20)
    vectorBytes = count * packedWordsPerVector * word64Bytes
    labelBytes = labelWordCount count * word64Bytes
    metaBytes = parsedNodeCount * nodeMetaFields * int32Bytes
    boundsBytes = parsedNodeCount * packedBoundWordsPerNode * word64Bytes

copyIndex :: Handle -> IndexLayout -> IO ReferenceIndex
copyIndex handle layout =
  case layoutFormat layout of
    KdV3 -> copyPackedIndex handle layout
    FlatV1 -> copyLegacyIndex handle layout
    KdV2 -> copyLegacyIndex handle layout

copyPackedIndex :: Handle -> IndexLayout -> IO ReferenceIndex
copyPackedIndex handle layout = do
  vectorWords <- readVector handle (layoutVectorBytes layout `div` word64Bytes) (layoutVectorBytes layout)
  labelWords <- readVector handle (layoutLabelBytes layout `div` word64Bytes) (layoutLabelBytes layout)
  nodeMeta <- readVector handle (layoutNodeCount layout * nodeMetaFields) (layoutMetaBytes layout)
  nodeBoundWords <- readVector handle (layoutBoundsBytes layout `div` word64Bytes) (layoutBoundsBytes layout)
  pure
    ReferenceIndex
      { referenceCount = layoutCount layout,
        referenceVectorWords = vectorWords,
        referenceLabelWords = labelWords,
        referenceNodeCount = layoutNodeCount layout,
        referenceNodeMeta = nodeMeta,
        referenceNodeBoundWords = nodeBoundWords
      }

copyLegacyIndex :: Handle -> IndexLayout -> IO ReferenceIndex
copyLegacyIndex handle layout = do
  vectors <- readVector handle (layoutCount layout * dimensionCount) (layoutVectorBytes layout)
  labels <- readVector handle (layoutCount layout) (layoutLabelBytes layout)
  nodeMeta <- readVector handle (layoutNodeCount layout * nodeMetaFields) (layoutMetaBytes layout)
  nodeBounds <- readVector handle (layoutNodeCount layout * dimensionCount * 2) (layoutBoundsBytes layout)
  vectorWords <- either fail pure (packReferenceVectors (layoutCount layout) vectors)
  labelWords <- either fail pure (packLabels (layoutCount layout) labels)
  nodeBoundWords <- either fail pure (packNodeBounds (layoutNodeCount layout) nodeBounds)
  pure
    ReferenceIndex
      { referenceCount = layoutCount layout,
        referenceVectorWords = vectorWords,
        referenceLabelWords = labelWords,
        referenceNodeCount = layoutNodeCount layout,
        referenceNodeMeta = nodeMeta,
        referenceNodeBoundWords = nodeBoundWords
      }

readVector :: (Storable a) => Handle -> Int -> Int -> IO (VS.Vector a)
readVector handle elementCount byteCount = do
  vector <- MVS.new elementCount
  MVS.unsafeWith vector $ \dst ->
    readExact handle (castPtr dst) byteCount
  VS.unsafeFreeze vector

readExact :: Handle -> Ptr a -> Int -> IO ()
readExact handle ptr total = loop 0
  where
    loop !readSoFar
      | readSoFar >= total = pure ()
      | otherwise = do
          chunk <- hGetBuf handle (ptr `plusPtr` readSoFar) (total - readSoFar)
          if chunk <= 0
            then fail "invalid reference index: unexpected end of file"
            else loop (readSoFar + chunk)

readWord64LE :: BS.ByteString -> Int -> Word64
readWord64LE bytes offset =
  foldr
    (.|.)
    0
    [fromIntegral (BS.index bytes (offset + i)) `shiftL` (8 * i) | i <- [0 .. 7]]

readWord32LE :: BS.ByteString -> Int -> Word32
readWord32LE bytes offset =
  foldr
    (.|.)
    0
    [fromIntegral (BS.index bytes (offset + i)) `shiftL` (8 * i) | i <- [0 .. 3]]

nodeLeft :: ReferenceIndex -> Int -> Int
nodeLeft index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields))
{-# INLINE nodeLeft #-}

nodeRight :: ReferenceIndex -> Int -> Int
nodeRight index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 1))
{-# INLINE nodeRight #-}

nodeStart :: ReferenceIndex -> Int -> Int
nodeStart index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 2))
{-# INLINE nodeStart #-}

nodeCount :: ReferenceIndex -> Int -> Int
nodeCount index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 3))
{-# INLINE nodeCount #-}

magicV1 :: BS.ByteString
magicV1 = "HLHMIDX1"

magicV2 :: BS.ByteString
magicV2 = "HLHMKD2!"

magicV3 :: BS.ByteString
magicV3 = "HLHMKD3!"

scaleMarker :: Word32
scaleMarker = 10000

headerSizeV1 :: Int
headerSizeV1 = 20

headerSizeV2 :: Int
headerSizeV2 = 28

headerSizeV3 :: Int
headerSizeV3 = 28

int16Bytes :: Int
int16Bytes = 2

int32Bytes :: Int
int32Bytes = 4

word64Bytes :: Int
word64Bytes = 8

nodeMetaFields :: Int
nodeMetaFields = 4

maxDistance :: Int64
maxDistance = maxBound
