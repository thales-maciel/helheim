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

import Data.Binary.Put
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int64)
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Helheim.Types
import Helheim.Vectorize (EncodedVector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import System.IO (Handle, IOMode (ReadMode), hFileSize, hGetBuf, withBinaryFile)

data ReferenceIndex = ReferenceIndex
  { referenceCount :: !Int,
    referenceVectors :: !(VS.Vector Int16),
    referenceLabels :: !(VS.Vector Word8)
  }
  deriving stock (Eq, Show)

loadIndex :: FilePath -> IO ReferenceIndex
loadIndex path =
  withBinaryFile path ReadMode $ \handle -> do
    header <- BS.hGet handle headerSize
    case headerLayout header of
      Left err -> fail ("invalid reference index: " <> err)
      Right layout -> do
        fileSize <- hFileSize handle
        if fileSize /= fromIntegral (layoutExpectedBytes layout)
          then fail "invalid reference index: unexpected file length"
          else copyIndex handle layout

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
  let (_, l1, _, l2, _, l3, _, l4, _, l5) =
        go 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0 maxDistance 0
      neighborCount = min 5 (referenceCount index)
      frauds = fromIntegral (l1 + l2 + l3 + l4 + l5)
   in if neighborCount == 0
        then 0
        else frauds / fromIntegral neighborCount
  where
    go !i !d1 !l1 !d2 !l2 !d3 !l3 !d4 !l4 !d5 !l5
      | i >= referenceCount index = (d1, l1, d2, l2, d3, l3, d4, l4, d5, l5)
      | otherwise =
          let d = squaredDistance index query i
              labelValue = fromIntegral (referenceLabels index VS.! i) :: Int
           in if d < d1
                then go (i + 1) d labelValue d1 l1 d2 l2 d3 l3 d4 l4
                else
                  if d < d2
                    then go (i + 1) d1 l1 d labelValue d2 l2 d3 l3 d4 l4
                    else
                      if d < d3
                        then go (i + 1) d1 l1 d2 l2 d labelValue d3 l3 d4 l4
                        else
                          if d < d4
                            then go (i + 1) d1 l1 d2 l2 d3 l3 d labelValue d4 l4
                            else
                              if d < d5
                                then go (i + 1) d1 l1 d2 l2 d3 l3 d4 l4 d labelValue
                                else go (i + 1) d1 l1 d2 l2 d3 l3 d4 l4 d5 l5

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

putIndex :: ReferenceIndex -> Put
putIndex index = do
  putByteString magic
  putWord64le (fromIntegral (referenceCount index))
  putWord32le scaleMarker
  VS.mapM_ putInt16le (referenceVectors index)
  VS.mapM_ putWord8 (referenceLabels index)

data IndexLayout = IndexLayout
  { layoutCount :: !Int,
    layoutVectorBytes :: !Int,
    layoutLabelBytes :: !Int,
    layoutExpectedBytes :: !Int
  }

headerLayout :: BS.ByteString -> Either String IndexLayout
headerLayout header
  | BS.length header /= headerSize = Left "file too small"
  | BS.take (BS.length magic) header /= magic = Left "bad magic"
  | readWord32LE header 16 /= scaleMarker = Left "unsupported scale"
  | otherwise =
      Right
        IndexLayout
          { layoutCount = count,
            layoutVectorBytes = vectorBytes,
            layoutLabelBytes = labelBytes,
            layoutExpectedBytes = expectedLength
          }
  where
    count = fromIntegral (readWord64LE header 8)
    vectorBytes = count * dimensions * int16Bytes
    labelBytes = count
    expectedLength = headerSize + vectorBytes + labelBytes

copyIndex :: Handle -> IndexLayout -> IO ReferenceIndex
copyIndex handle layout = do
  vectors <- MVS.new (layoutCount layout * dimensions)
  labels <- MVS.new (layoutCount layout)
  MVS.unsafeWith vectors $ \dst ->
    readExact handle (castPtr dst) (layoutVectorBytes layout)
  MVS.unsafeWith labels $ \dst ->
    readExact handle (castPtr dst) (layoutLabelBytes layout)
  frozenVectors <- VS.unsafeFreeze vectors
  frozenLabels <- VS.unsafeFreeze labels
  pure
    ReferenceIndex
      { referenceCount = layoutCount layout,
        referenceVectors = frozenVectors,
        referenceLabels = frozenLabels
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

magic :: BS.ByteString
magic = "HLHMIDX1"

scaleMarker :: Word32
scaleMarker = 10000

headerSize :: Int
headerSize = 20

int16Bytes :: Int
int16Bytes = 2

dimensions :: Int
dimensions = 14

maxDistance :: Int64
maxDistance = maxBound
