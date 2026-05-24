{-# LANGUAGE DerivingStrategies #-}

module Helheim.PackedVector
  ( PackedVector (..),
    decodeDimAt,
    labelAt,
    labelWordCount,
    packEncodedList,
    packEncodedVector,
    packLabels,
    packNodeBounds,
    packReferenceVectors,
    packedBoundWordsPerNode,
    packedDimensionAt,
    packedDimensionAtIndex,
    packedLowerBound,
    packedLowerBoundRef,
    packedLowerBoundUnder,
    packedSquaredDistance,
    packedSquaredDistanceRef,
    packedSquaredDistanceUnder,
    packedWordsPerVector,
  )
where

import Control.Monad.ST (runST)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, setBit, testBit)
import Data.Int (Int16, Int64)
import Data.List (elemIndex)
import Data.Word (Word64, Word8)
import Helheim.Features (Dimension (..), EncodedVector, dimensionCount)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS

data PackedVector = PackedVector
  { packedLowWord :: {-# UNPACK #-} !Word64,
    packedHighWord :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Show)

packedWordsPerVector :: Int
packedWordsPerVector = 2

packedBoundWordsPerNode :: Int
packedBoundWordsPerNode = 4

packEncodedVector :: EncodedVector -> Either String PackedVector
packEncodedVector vector
  | VS.length vector /= dimensionCount =
      Left ("encoded vector has " <> show (VS.length vector) <> " dimensions")
  | otherwise = packVectorAt vector 0

packEncodedList :: [Int16] -> Either String PackedVector
packEncodedList values =
  packEncodedVector (VS.fromListN dimensionCount values)

packReferenceVectors :: Int -> VS.Vector Int16 -> Either String (VS.Vector Word64)
packReferenceVectors count refs
  | VS.length refs /= count * dimensionCount =
      Left
        ( "reference vector length mismatch: expected "
            <> show (count * dimensionCount)
            <> ", got "
            <> show (VS.length refs)
        )
  | otherwise = runST $ do
      out <- MVS.new (count * packedWordsPerVector)
      let go !row
            | row >= count = Right <$> VS.unsafeFreeze out
            | otherwise =
                case packVectorAt refs (row * dimensionCount) of
                  Left err ->
                    pure (Left ("reference row " <> show row <> ": " <> err))
                  Right (PackedVector lo hi) -> do
                    let base = row * packedWordsPerVector
                    MVS.unsafeWrite out base lo
                    MVS.unsafeWrite out (base + 1) hi
                    go (row + 1)
      go 0

packNodeBounds :: Int -> VS.Vector Int16 -> Either String (VS.Vector Word64)
packNodeBounds nodeCount bounds
  | VS.length bounds /= nodeCount * dimensionCount * 2 =
      Left
        ( "node bounds length mismatch: expected "
            <> show (nodeCount * dimensionCount * 2)
            <> ", got "
            <> show (VS.length bounds)
        )
  | otherwise = runST $ do
      out <- MVS.new (nodeCount * packedBoundWordsPerNode)
      let go !node
            | node >= nodeCount = Right <$> VS.unsafeFreeze out
            | otherwise = do
                let sourceBase = node * dimensionCount * 2
                case (packVectorAt bounds sourceBase, packVectorAt bounds (sourceBase + dimensionCount)) of
                  (Right (PackedVector loMin hiMin), Right (PackedVector loMax hiMax)) -> do
                    let outBase = node * packedBoundWordsPerNode
                    MVS.unsafeWrite out outBase loMin
                    MVS.unsafeWrite out (outBase + 1) hiMin
                    MVS.unsafeWrite out (outBase + 2) loMax
                    MVS.unsafeWrite out (outBase + 3) hiMax
                    go (node + 1)
                  (Left err, _) ->
                    pure (Left ("node " <> show node <> " min bounds: " <> err))
                  (_, Left err) ->
                    pure (Left ("node " <> show node <> " max bounds: " <> err))
      go 0

packLabels :: Int -> VS.Vector Word8 -> Either String (VS.Vector Word64)
packLabels count labels
  | VS.length labels /= count =
      Left ("label length mismatch: expected " <> show count <> ", got " <> show (VS.length labels))
  | otherwise = runST $ do
      out <- MVS.replicate (labelWordCount count) 0
      let go !row
            | row >= count = Right <$> VS.unsafeFreeze out
            | otherwise = do
                case labels VS.! row of
                  0 -> go (row + 1)
                  1 -> do
                    let wordIndex = row `div` bitsPerWord
                        bitIndex = row `rem` bitsPerWord
                    word <- MVS.unsafeRead out wordIndex
                    MVS.unsafeWrite out wordIndex (setBit word bitIndex)
                    go (row + 1)
                  value ->
                    pure (Left ("label row " <> show row <> " is not 0 or 1: " <> show value))
      go 0

labelAt :: VS.Vector Word64 -> Int -> Int
labelAt labels row =
  if testBit (labels VS.! wordIndex) bitIndex then 1 else 0
  where
    wordIndex = row `div` bitsPerWord
    bitIndex = row `rem` bitsPerWord

labelWordCount :: Int -> Int
labelWordCount count =
  (count + bitsPerWord - 1) `div` bitsPerWord

packedDimensionAt :: PackedVector -> Dimension -> Int16
packedDimensionAt packed =
  packedDimensionAtIndex packed . fromEnum

packedDimensionAtIndex :: PackedVector -> Int -> Int16
packedDimensionAtIndex packed dim =
  decodeField (fieldSpecAt dim) (extractField packed (fieldSpecAt dim))

-- | Decode a single packed dimension. Fully unrolled per dimension using the
-- compile-time bit layout from 'fieldSpecAt'; computes exactly the same value
-- as @'packedDimensionAtIndex'@ but with no 'FieldSpec' allocation and O(1)
-- (instead of list @!!@) finite-codebook lookups. Marked INLINE so that at a
-- call site with a statically-known dimension the case collapses to one arm.
decodeDimAt :: PackedVector -> Int -> Int16
decodeDimAt (PackedVector lo hi) dim =
  case dim of
    0 -> fromIntegral (lo .&. 0x3FFF)
    1 -> VS.unsafeIndex installmentTable (fromIntegral ((lo `shiftR` 14) .&. 0xF))
    2 -> fromIntegral ((lo `shiftR` 18) .&. 0x3FFF)
    3 -> VS.unsafeIndex hourTable (fromIntegral ((lo `shiftR` 32) .&. 0x1F))
    4 -> VS.unsafeIndex weekdayTable (fromIntegral ((lo `shiftR` 37) .&. 0x7))
    5 -> decodeMaybeMissing ((lo `shiftR` 40) .&. 0x3FFF)
    6 -> decodeMaybeMissing (hi .&. 0x3FFF)
    7 -> fromIntegral ((hi `shiftR` 14) .&. 0x3FFF)
    8 -> VS.unsafeIndex txCountTable (fromIntegral ((lo `shiftR` 54) .&. 0x1F))
    9 -> VS.unsafeIndex flagTable (fromIntegral ((lo `shiftR` 59) .&. 0x1))
    10 -> VS.unsafeIndex flagTable (fromIntegral ((lo `shiftR` 60) .&. 0x1))
    11 -> VS.unsafeIndex flagTable (fromIntegral ((lo `shiftR` 61) .&. 0x1))
    12 -> VS.unsafeIndex mccRiskTable (fromIntegral ((hi `shiftR` 28) .&. 0xF))
    13 -> fromIntegral ((hi `shiftR` 32) .&. 0x3FFF)
    _ -> error ("unknown packed dimension index: " <> show dim)
  where
    decodeMaybeMissing code
      | code == missingCode = missingEncoded
      | otherwise = fromIntegral code
{-# INLINE decodeDimAt #-}

-- | Squared Euclidean distance between the query and packed reference @row@.
-- Fully unrolled; the query is read with 'VS.unsafeIndex' (always length
-- 'dimensionCount', built by 'Helheim.Features.encodeFraudFeatures').
packedSquaredDistance :: VS.Vector Word64 -> EncodedVector -> Int -> Int64
packedSquaredDistance refs query row =
  sq 0 + sq 1 + sq 2 + sq 3 + sq 4 + sq 5 + sq 6
    + sq 7 + sq 8 + sq 9 + sq 10 + sq 11 + sq 12 + sq 13
  where
    packed = packedVectorAt refs row
    sq !d =
      let q = fromIntegral (VS.unsafeIndex query d) :: Int64
          r = fromIntegral (decodeDimAt packed d) :: Int64
          diff = q - r
       in diff * diff
    {-# INLINE sq #-}
{-# INLINABLE packedSquaredDistance #-}

-- | Cutoff-aware squared distance: accumulates in the same order and stops as
-- soon as @acc >= cutoff@, returning that @acc@ (which is then @>= cutoff@).
-- Exact: every term is non-negative so the accumulator is monotonic, hence if
-- the true distance is @< cutoff@ no early-out fires and the exact value is
-- returned; otherwise the result is @>= cutoff@, which is all the caller's
-- threshold comparison needs.
packedSquaredDistanceUnder :: VS.Vector Word64 -> EncodedVector -> Int -> Int64 -> Int64
packedSquaredDistanceUnder refs query row cutoff =
  stop (sq 0) (\a0 ->
  stop (a0 + sq 1) (\a1 ->
  stop (a1 + sq 2) (\a2 ->
  stop (a2 + sq 3) (\a3 ->
  stop (a3 + sq 4) (\a4 ->
  stop (a4 + sq 5) (\a5 ->
  stop (a5 + sq 6) (\a6 ->
  stop (a6 + sq 7) (\a7 ->
  stop (a7 + sq 8) (\a8 ->
  stop (a8 + sq 9) (\a9 ->
  stop (a9 + sq 10) (\a10 ->
  stop (a10 + sq 11) (\a11 ->
  stop (a11 + sq 12) (\a12 ->
  a12 + sq 13)))))))))))))
  where
    packed = packedVectorAt refs row
    sq !d =
      let q = fromIntegral (VS.unsafeIndex query d) :: Int64
          r = fromIntegral (decodeDimAt packed d) :: Int64
          diff = q - r
       in diff * diff
    {-# INLINE sq #-}
    stop !acc k = if acc >= cutoff then acc else k acc
    {-# INLINE stop #-}
{-# INLINABLE packedSquaredDistanceUnder #-}

-- | Lower bound of the query distance to the bounding box of @node@.
packedLowerBound :: VS.Vector Word64 -> EncodedVector -> Int -> Int64
packedLowerBound bounds query node =
  lb 0 + lb 1 + lb 2 + lb 3 + lb 4 + lb 5 + lb 6
    + lb 7 + lb 8 + lb 9 + lb 10 + lb 11 + lb 12 + lb 13
  where
    base = node * packedBoundWordsPerNode
    mins = PackedVector (bounds VS.! base) (bounds VS.! (base + 1))
    maxs = PackedVector (bounds VS.! (base + 2)) (bounds VS.! (base + 3))
    lb !d =
      let q = fromIntegral (VS.unsafeIndex query d) :: Int64
          lo = fromIntegral (decodeDimAt mins d) :: Int64
          hi = fromIntegral (decodeDimAt maxs d) :: Int64
          diff
            | q < lo = lo - q
            | q > hi = q - hi
            | otherwise = 0
       in diff * diff
    {-# INLINE lb #-}
{-# INLINABLE packedLowerBound #-}

-- | Cutoff-aware lower bound; see 'packedSquaredDistanceUnder' for exactness.
packedLowerBoundUnder :: VS.Vector Word64 -> EncodedVector -> Int -> Int64 -> Int64
packedLowerBoundUnder bounds query node cutoff =
  stop (lb 0) (\a0 ->
  stop (a0 + lb 1) (\a1 ->
  stop (a1 + lb 2) (\a2 ->
  stop (a2 + lb 3) (\a3 ->
  stop (a3 + lb 4) (\a4 ->
  stop (a4 + lb 5) (\a5 ->
  stop (a5 + lb 6) (\a6 ->
  stop (a6 + lb 7) (\a7 ->
  stop (a7 + lb 8) (\a8 ->
  stop (a8 + lb 9) (\a9 ->
  stop (a9 + lb 10) (\a10 ->
  stop (a10 + lb 11) (\a11 ->
  stop (a11 + lb 12) (\a12 ->
  a12 + lb 13)))))))))))))
  where
    base = node * packedBoundWordsPerNode
    mins = PackedVector (bounds VS.! base) (bounds VS.! (base + 1))
    maxs = PackedVector (bounds VS.! (base + 2)) (bounds VS.! (base + 3))
    lb !d =
      let q = fromIntegral (VS.unsafeIndex query d) :: Int64
          lo = fromIntegral (decodeDimAt mins d) :: Int64
          hi = fromIntegral (decodeDimAt maxs d) :: Int64
          diff
            | q < lo = lo - q
            | q > hi = q - hi
            | otherwise = 0
       in diff * diff
    {-# INLINE lb #-}
    stop !acc k = if acc >= cutoff then acc else k acc
    {-# INLINE stop #-}
{-# INLINABLE packedLowerBoundUnder #-}

-- | Generic loop-based squared distance, retained verbatim as a test oracle
-- for 'packedSquaredDistance'. Do not use on the hot path.
packedSquaredDistanceRef :: VS.Vector Word64 -> EncodedVector -> Int -> Int64
packedSquaredDistanceRef refs query row =
  loop 0 0
  where
    packed = packedVectorAt refs row
    loop !dim !acc
      | dim >= dimensionCount = acc
      | otherwise =
          let q = fromIntegral (query VS.! dim) :: Int64
              r = fromIntegral (packedDimensionAtIndex packed dim) :: Int64
              diff = q - r
           in loop (dim + 1) (acc + diff * diff)

-- | Generic loop-based lower bound, retained verbatim as a test oracle for
-- 'packedLowerBound'. Do not use on the hot path.
packedLowerBoundRef :: VS.Vector Word64 -> EncodedVector -> Int -> Int64
packedLowerBoundRef bounds query node =
  loop 0 0
  where
    base = node * packedBoundWordsPerNode
    mins = PackedVector (bounds VS.! base) (bounds VS.! (base + 1))
    maxs = PackedVector (bounds VS.! (base + 2)) (bounds VS.! (base + 3))
    loop !dim !acc
      | dim >= dimensionCount = acc
      | otherwise =
          let q = fromIntegral (query VS.! dim) :: Int64
              lo = fromIntegral (packedDimensionAtIndex mins dim) :: Int64
              hi = fromIntegral (packedDimensionAtIndex maxs dim) :: Int64
              diff
                | q < lo = lo - q
                | q > hi = q - hi
                | otherwise = 0
           in loop (dim + 1) (acc + diff * diff)

packVectorAt :: VS.Vector Int16 -> Int -> Either String PackedVector
packVectorAt vector offset =
  go 0 0 0
  where
    go !dim !lo !hi
      | dim >= dimensionCount = Right (PackedVector lo hi)
      | otherwise = do
          let spec = fieldSpecAt dim
          code <- encodeField spec (vector VS.! (offset + dim))
          let (lo', hi') = insertField spec code lo hi
          go (dim + 1) lo' hi'

packedVectorAt :: VS.Vector Word64 -> Int -> PackedVector
packedVectorAt refs row =
  PackedVector (refs VS.! base) (refs VS.! (base + 1))
  where
    base = row * packedWordsPerVector

data FieldSpec = FieldSpec
  { fieldOffset :: !Int,
    fieldWidth :: !Int,
    fieldCodec :: !FieldCodec
  }

data FieldCodec
  = GenericValue
  | GenericMaybeMissing
  | FiniteValues [Int16]

fieldSpecAt :: Int -> FieldSpec
fieldSpecAt 0 = FieldSpec 0 14 GenericValue
fieldSpecAt 1 = FieldSpec 14 4 (FiniteValues installmentValues)
fieldSpecAt 2 = FieldSpec 18 14 GenericValue
fieldSpecAt 3 = FieldSpec 32 5 (FiniteValues hourValues)
fieldSpecAt 4 = FieldSpec 37 3 (FiniteValues weekdayValues)
fieldSpecAt 5 = FieldSpec 40 14 GenericMaybeMissing
fieldSpecAt 6 = FieldSpec 64 14 GenericMaybeMissing
fieldSpecAt 7 = FieldSpec 78 14 GenericValue
fieldSpecAt 8 = FieldSpec 54 5 (FiniteValues txCountValues)
fieldSpecAt 9 = FieldSpec 59 1 (FiniteValues flagValues)
fieldSpecAt 10 = FieldSpec 60 1 (FiniteValues flagValues)
fieldSpecAt 11 = FieldSpec 61 1 (FiniteValues flagValues)
fieldSpecAt 12 = FieldSpec 92 4 (FiniteValues mccRiskValues)
fieldSpecAt 13 = FieldSpec 96 14 GenericValue
fieldSpecAt dim = error ("unknown packed dimension index: " <> show dim)

encodeField :: FieldSpec -> Int16 -> Either String Word64
encodeField FieldSpec {fieldCodec = GenericValue} value =
  genericCode False value
encodeField FieldSpec {fieldCodec = GenericMaybeMissing} value =
  genericCode True value
encodeField FieldSpec {fieldCodec = FiniteValues values} value =
  case elemIndex value values of
    Just code -> Right (fromIntegral code)
    Nothing -> Left ("value " <> show value <> " is not in finite codebook " <> show values)

decodeField :: FieldSpec -> Word64 -> Int16
decodeField FieldSpec {fieldCodec = GenericValue} code =
  fromIntegral code
decodeField FieldSpec {fieldCodec = GenericMaybeMissing} code
  | code == missingCode = missingEncoded
  | otherwise = fromIntegral code
decodeField FieldSpec {fieldCodec = FiniteValues values} code =
  values !! fromIntegral code

genericCode :: Bool -> Int16 -> Either String Word64
genericCode allowMissing value
  | allowMissing && value == missingEncoded = Right missingCode
  | value >= 0 && value <= maxEncoded = Right (fromIntegral value)
  | otherwise = Left ("generic encoded value out of range: " <> show value)

insertField :: FieldSpec -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
insertField spec code lo hi
  | code > fieldMask (fieldWidth spec) =
      error ("packed field overflow at offset " <> show (fieldOffset spec))
  | fieldOffset spec < bitsPerWord =
      (lo .|. (code `shiftL` fieldOffset spec), hi)
  | otherwise =
      (lo, hi .|. (code `shiftL` (fieldOffset spec - bitsPerWord)))

extractField :: PackedVector -> FieldSpec -> Word64
extractField (PackedVector lo hi) spec
  | fieldOffset spec < bitsPerWord =
      (lo `shiftR` fieldOffset spec) .&. fieldMask (fieldWidth spec)
  | otherwise =
      (hi `shiftR` (fieldOffset spec - bitsPerWord)) .&. fieldMask (fieldWidth spec)

fieldMask :: Int -> Word64
fieldMask width =
  (1 `shiftL` width) - 1

linearValues :: Int -> Int -> [Int16]
linearValues denominator maxValue =
  [round (fromIntegral value / fromIntegral denominator * encodedScale) | value <- [0 .. maxValue]]

installmentValues :: [Int16]
installmentValues = linearValues 12 12

hourValues :: [Int16]
hourValues = linearValues 23 23

weekdayValues :: [Int16]
weekdayValues = linearValues 6 6

txCountValues :: [Int16]
txCountValues = linearValues 20 20

flagValues :: [Int16]
flagValues = [0, maxEncoded]

mccRiskValues :: [Int16]
mccRiskValues = [1500, 3000, 2000, 4500, 8000, 7500, 8500, 3500, 2500, 5000]

-- O(1)-indexable codebooks for 'decodeDimAt', built directly from the list
-- codebooks above so the decoded values are provably identical to the generic
-- @values !! code@ path. NOINLINE keeps each as a shared, build-once CAF.
installmentTable :: VS.Vector Int16
installmentTable = VS.fromList installmentValues
{-# NOINLINE installmentTable #-}

hourTable :: VS.Vector Int16
hourTable = VS.fromList hourValues
{-# NOINLINE hourTable #-}

weekdayTable :: VS.Vector Int16
weekdayTable = VS.fromList weekdayValues
{-# NOINLINE weekdayTable #-}

txCountTable :: VS.Vector Int16
txCountTable = VS.fromList txCountValues
{-# NOINLINE txCountTable #-}

flagTable :: VS.Vector Int16
flagTable = VS.fromList flagValues
{-# NOINLINE flagTable #-}

mccRiskTable :: VS.Vector Int16
mccRiskTable = VS.fromList mccRiskValues
{-# NOINLINE mccRiskTable #-}

encodedScale :: Double
encodedScale = 10000

maxEncoded :: Int16
maxEncoded = 10000

missingEncoded :: Int16
missingEncoded = -10000

missingCode :: Word64
missingCode = 10001

bitsPerWord :: Int
bitsPerWord = 64
