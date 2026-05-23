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
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Helheim.Types
import Helheim.Vectorize (EncodedVector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import System.IO (Handle, IOMode (ReadMode), SeekMode (AbsoluteSeek), hFileSize, hGetBuf, hSeek, withBinaryFile)

data ReferenceIndex = ReferenceIndex
  { referenceCount :: !Int,
    referenceVectors :: !(VS.Vector Int16),
    referenceLabels :: !(VS.Vector Word8),
    referenceNodeCount :: !Int,
    referenceNodeMeta :: !(VS.Vector Int32),
    referenceNodeBounds :: !(VS.Vector Int16)
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
  let (_, l1, _, l2, _, l3, _, l4, _, l5) =
        go 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0
   in l1 + l2 + l3 + l4 + l5
  where
    go !i !d1 !l1 !d2 !l2 !d3 !l3 !d4 !l4 !d5 !l5
      | i >= referenceCount index = (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
      | otherwise =
          let d = squaredDistance index query i
              labelValue = fromIntegral (referenceLabels index VS.! i) :: Int
           in insertBest d labelValue d1 l1 d2 l2 d3 l3 d4 l4 d5 l5
                `seqTuple` \(d1', l1', d2', l2', d3', l3', d4', l4', d5', l5') ->
                  go (i + 1) d1' l1' d2' l2' d3' l3' d4' l4' d5' l5'

kdFrauds :: ReferenceIndex -> EncodedVector -> Int
kdFrauds index query =
  let (_, l1, _, l2, _, l3, _, l4, _, l5) =
        goNode 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0
   in l1 + l2 + l3 + l4 + l5
  where
    goNode !node !d1 !l1 !d2 !l2 !d3 !l3 !d4 !l4 !d5 !l5
      | node < 0 || node >= referenceNodeCount index =
          (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
      | lowerBound index query node >= d5 =
          (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
      | nodeLeft index node < 0 =
          scanLeaf (nodeStart index node) (nodeCount index node) d1 l1 d2 l2 d3 l3 d4 l4 d5 l5
      | otherwise =
          let left = nodeLeft index node
              right = nodeRight index node
              lbLeft = lowerBound index query left
              lbRight = lowerBound index query right
              (first, firstLb, second, secondLb) =
                if lbLeft <= lbRight
                  then (left, lbLeft, right, lbRight)
                  else (right, lbRight, left, lbLeft)
              bestAfterFirst =
                if firstLb < d5
                  then goNode first d1 l1 d2 l2 d3 l3 d4 l4 d5 l5
                  else (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
           in bestAfterFirst
                `seqTuple` \(fd1, fl1, fd2, fl2, fd3, fl3, fd4, fl4, fd5, fl5) ->
                  if secondLb < fd5
                    then goNode second fd1 fl1 fd2 fl2 fd3 fl3 fd4 fl4 fd5 fl5
                    else bestAfterFirst

    scanLeaf !start !count !d1 !l1 !d2 !l2 !d3 !l3 !d4 !l4 !d5 !l5 =
      loop start (start + count) d1 l1 d2 l2 d3 l3 d4 l4 d5 l5

    loop !i !end !d1 !l1 !d2 !l2 !d3 !l3 !d4 !l4 !d5 !l5
      | i >= end = (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
      | otherwise =
          let d = squaredDistance index query i
              labelValue = fromIntegral (referenceLabels index VS.! i) :: Int
           in insertBest d labelValue d1 l1 d2 l2 d3 l3 d4 l4 d5 l5
                `seqTuple` \(d1', l1', d2', l2', d3', l3', d4', l4', d5', l5') ->
                  loop (i + 1) end d1' l1' d2' l2' d3' l3' d4' l4' d5' l5'

insertBest ::
  Int64 ->
  Int ->
  Int64 ->
  Int ->
  Int64 ->
  Int ->
  Int64 ->
  Int ->
  Int64 ->
  Int ->
  Int64 ->
  Int ->
  (Int64, Int, Int64, Int, Int64, Int, Int64, Int, Int64, Int)
insertBest d labelValue d1 l1 d2 l2 d3 l3 d4 l4 d5 l5
  | d < d1 = (d, labelValue, d1, l1, d2, l2, d3, l3, d4, l4)
  | d < d2 = (d1, l1, d, labelValue, d2, l2, d3, l3, d4, l4)
  | d < d3 = (d1, l1, d2, l2, d, labelValue, d3, l3, d4, l4)
  | d < d4 = (d1, l1, d2, l2, d3, l3, d, labelValue, d4, l4)
  | d < d5 = (d1, l1, d2, l2, d3, l3, d4, l4, d, labelValue)
  | otherwise = (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)

seqTuple ::
  (Int64, Int, Int64, Int, Int64, Int, Int64, Int, Int64, Int) ->
  ((Int64, Int, Int64, Int, Int64, Int, Int64, Int, Int64, Int) -> a) ->
  a
seqTuple t@(a, b, c, d, e, f, g, h, i, j) k =
  a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` h `seq` i `seq` j `seq` k t

squaredDistance :: ReferenceIndex -> EncodedVector -> Int -> Int64
squaredDistance index query row =
  loop 0 0
  where
    offset = row * dimensions
    refs = referenceVectors index
    loop !dim !acc
      | dim >= dimensions = acc
      | otherwise =
          let q = fromIntegral (query VS.! dim) :: Int64
              r = fromIntegral (refs VS.! (offset + dim)) :: Int64
              diff = q - r
           in loop (dim + 1) (acc + diff * diff)

lowerBound :: ReferenceIndex -> EncodedVector -> Int -> Int64
lowerBound index query node =
  loop 0 0
  where
    base = node * dimensions * 2
    bounds = referenceNodeBounds index
    loop !dim !acc
      | dim >= dimensions = acc
      | otherwise =
          let q = fromIntegral (query VS.! dim) :: Int64
              lo = fromIntegral (bounds VS.! (base + dim)) :: Int64
              hi = fromIntegral (bounds VS.! (base + dimensions + dim)) :: Int64
              diff
                | q < lo = lo - q
                | q > hi = q - hi
                | otherwise = 0
           in loop (dim + 1) (acc + diff * diff)

putIndex :: ReferenceIndex -> Put
putIndex index = do
  putByteString magicV2
  putWord64le (fromIntegral (referenceCount index))
  putWord32le scaleMarker
  putWord32le (fromIntegral (referenceNodeCount index))
  putWord32le (fromIntegral dimensions)
  VS.mapM_ putInt16le (referenceVectors index)
  VS.mapM_ putWord8 (referenceLabels index)
  VS.mapM_ putInt32le (referenceNodeMeta index)
  VS.mapM_ putInt16le (referenceNodeBounds index)

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

data IndexFormat = FlatV1 | KdV2
  deriving stock (Eq, Show)

parseHeader :: BS.ByteString -> Either String IndexLayout
parseHeader header
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
    vectorBytes = count * dimensions * int16Bytes
    labelBytes = count

headerLayoutV2 :: BS.ByteString -> Either String IndexLayout
headerLayoutV2 header
  | BS.length header /= headerSizeV2 = Left "file too small"
  | readWord32LE header 16 /= scaleMarker = Left "unsupported scale"
  | readWord32LE header 24 /= fromIntegral dimensions = Left "unsupported dimensions"
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
    vectorBytes = count * dimensions * int16Bytes
    labelBytes = count
    metaBytes = parsedNodeCount * nodeMetaFields * int32Bytes
    boundsBytes = parsedNodeCount * dimensions * 2 * int16Bytes

copyIndex :: Handle -> IndexLayout -> IO ReferenceIndex
copyIndex handle layout = do
  vectors <- MVS.new (layoutCount layout * dimensions)
  labels <- MVS.new (layoutCount layout)
  nodeMeta <- MVS.new (layoutNodeCount layout * nodeMetaFields)
  nodeBounds <- MVS.new (layoutNodeCount layout * dimensions * 2)
  MVS.unsafeWith vectors $ \dst ->
    readExact handle (castPtr dst) (layoutVectorBytes layout)
  MVS.unsafeWith labels $ \dst ->
    readExact handle (castPtr dst) (layoutLabelBytes layout)
  when (layoutFormat layout == KdV2) $ do
    MVS.unsafeWith nodeMeta $ \dst ->
      readExact handle (castPtr dst) (layoutMetaBytes layout)
    MVS.unsafeWith nodeBounds $ \dst ->
      readExact handle (castPtr dst) (layoutBoundsBytes layout)
  frozenVectors <- VS.unsafeFreeze vectors
  frozenLabels <- VS.unsafeFreeze labels
  frozenNodeMeta <- VS.unsafeFreeze nodeMeta
  frozenNodeBounds <- VS.unsafeFreeze nodeBounds
  pure
    ReferenceIndex
      { referenceCount = layoutCount layout,
        referenceVectors = frozenVectors,
        referenceLabels = frozenLabels,
        referenceNodeCount = layoutNodeCount layout,
        referenceNodeMeta = frozenNodeMeta,
        referenceNodeBounds = frozenNodeBounds
      }

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

nodeRight :: ReferenceIndex -> Int -> Int
nodeRight index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 1))

nodeStart :: ReferenceIndex -> Int -> Int
nodeStart index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 2))

nodeCount :: ReferenceIndex -> Int -> Int
nodeCount index node = fromIntegral (referenceNodeMeta index VS.! (node * nodeMetaFields + 3))

magicV1 :: BS.ByteString
magicV1 = "HLHMIDX1"

magicV2 :: BS.ByteString
magicV2 = "HLHMKD2!"

scaleMarker :: Word32
scaleMarker = 10000

headerSizeV1 :: Int
headerSizeV1 = 20

headerSizeV2 :: Int
headerSizeV2 = 28

int16Bytes :: Int
int16Bytes = 2

int32Bytes :: Int
int32Bytes = 4

nodeMetaFields :: Int
nodeMetaFields = 4

dimensions :: Int
dimensions = 14

maxDistance :: Int64
maxDistance = maxBound
