{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Helheim.ReferenceBuilder
  ( BuildStats (..),
    buildIndexFromGzip,
    buildIndexFromJsonBytes,
    countReferenceVectors,
  )
where

import qualified Codec.Compression.GZip as GZip
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Int (Int16, Int32)
import Data.Word (Word8)
import Helheim.Features (dimensionCount, encodeDimension)
import Helheim.Index
import Helheim.PackedVector
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data BuildStats = BuildStats
  { buildStatsReferences :: !Int,
    buildStatsFrauds :: !Int,
    buildStatsLegits :: !Int
  }
  deriving stock (Eq, Show)

buildIndexFromGzip :: FilePath -> FilePath -> Int -> IO BuildStats
buildIndexFromGzip input output leafSize = do
  compressed <- BL.readFile input
  let jsonBytes = BL.toStrict (GZip.decompress compressed)
  buildIndexFromJsonBytes output leafSize jsonBytes

buildIndexFromJsonBytes :: FilePath -> Int -> BS.ByteString -> IO BuildStats
buildIndexFromJsonBytes output leafSize jsonBytes = do
  let count = countReferenceVectors jsonBytes
  hPutStrLn stderr ("building reference index for " <> show count <> " vectors")
  vectors <- MVS.new (count * dimensionCount)
  labels <- MVS.new count
  stats <- fillVectors jsonBytes count vectors labels
  frozenVectors <- VS.unsafeFreeze vectors
  frozenLabels <- VS.unsafeFreeze labels
  index <- buildKdIndex leafSize count frozenVectors frozenLabels
  saveIndex
    output
    index
  pure stats

buildKdIndex :: Int -> Int -> VS.Vector Int16 -> VS.Vector Word8 -> IO ReferenceIndex
buildKdIndex leafSize count originalVectors originalLabels = do
  hPutStrLn stderr ("building kd-tree index with leaf size " <> show leafSize)
  indices <- MVS.new count
  forInt 0 count $ \i ->
    MVS.unsafeWrite indices i (fromIntegral i)
  let maxLeaves = nextPowerOfTwo ((count + leafSize - 1) `div` leafSize)
      maxNodes = 2 * maxLeaves + 1
  nodeMeta <- MVS.replicate (maxNodes * nodeMetaFields) 0
  nodeBounds <- MVS.new (maxNodes * dimensionCount * 2)
  nextNode <- newIORef 0
  _ <- buildNode leafSize originalVectors indices nodeMeta nodeBounds nextNode 0 count
  nodeCount <- readIORef nextNode
  hPutStrLn stderr ("kd-tree nodes: " <> show nodeCount)
  reorderedVectors <- MVS.new (count * dimensionCount)
  reorderedLabels <- MVS.new count
  forInt 0 count $ \newRow -> do
    oldRow <- fromIntegral <$> MVS.unsafeRead indices newRow
    forInt 0 dimensionCount $ \dim ->
      MVS.unsafeWrite reorderedVectors (newRow * dimensionCount + dim) (refValue originalVectors oldRow dim)
    MVS.unsafeWrite reorderedLabels newRow (originalLabels VS.! oldRow)
  finalVectors <- VS.unsafeFreeze reorderedVectors
  finalLabels <- VS.unsafeFreeze reorderedLabels
  finalMetaFull <- VS.unsafeFreeze nodeMeta
  finalBoundsFull <- VS.unsafeFreeze nodeBounds
  let finalMeta = VS.take (nodeCount * nodeMetaFields) finalMetaFull
      finalBounds = VS.take (nodeCount * dimensionCount * 2) finalBoundsFull
  packedVectors <- either fail pure (packReferenceVectors count finalVectors)
  packedLabels <- either fail pure (packLabels count finalLabels)
  packedBounds <- either fail pure (packNodeBounds nodeCount finalBounds)
  pure
    ReferenceIndex
      { referenceCount = count,
        referenceVectorWords = packedVectors,
        referenceLabelWords = packedLabels,
        referenceNodeCount = nodeCount,
        referenceNodeMeta = finalMeta,
        referenceNodeBoundWords = packedBounds
      }

buildNode ::
  Int ->
  VS.Vector Int16 ->
  MVS.IOVector Int32 ->
  MVS.IOVector Int32 ->
  MVS.IOVector Int16 ->
  IORef Int ->
  Int ->
  Int ->
  IO Int
buildNode leafSize refs indices nodeMeta nodeBounds nextNode start count = do
  node <- allocNode nextNode
  (mins, maxs, splitDim, splitWidth) <- computeBounds refs indices start count
  writeNodeBounds nodeBounds node mins maxs
  if count <= leafSize || splitWidth == 0
    then do
      writeMeta nodeMeta node (-1) (-1) start count
      pure node
    else do
      let median = start + count `div` 2
      selectByDim refs indices start (start + count) median splitDim
      left <- buildNode leafSize refs indices nodeMeta nodeBounds nextNode start (median - start)
      right <- buildNode leafSize refs indices nodeMeta nodeBounds nextNode median (start + count - median)
      writeMeta nodeMeta node left right 0 0
      pure node

allocNode :: IORef Int -> IO Int
allocNode ref = do
  node <- readIORef ref
  writeIORef ref (node + 1)
  pure node

computeBounds ::
  VS.Vector Int16 ->
  MVS.IOVector Int32 ->
  Int ->
  Int ->
  IO (VS.Vector Int16, VS.Vector Int16, Int, Int)
computeBounds refs indices start count = do
  firstRow <- fromIntegral <$> MVS.unsafeRead indices start
  mins <- MVS.new dimensionCount
  maxs <- MVS.new dimensionCount
  forInt 0 dimensionCount $ \dim -> do
    let value = refValue refs firstRow dim
    MVS.unsafeWrite mins dim value
    MVS.unsafeWrite maxs dim value
  forInt (start + 1) (start + count) $ \pos -> do
    row <- fromIntegral <$> MVS.unsafeRead indices pos
    forInt 0 dimensionCount $ \dim -> do
      let value = refValue refs row dim
      oldMin <- MVS.unsafeRead mins dim
      oldMax <- MVS.unsafeRead maxs dim
      if value < oldMin then MVS.unsafeWrite mins dim value else pure ()
      if value > oldMax then MVS.unsafeWrite maxs dim value else pure ()
  minsFrozen <- VS.unsafeFreeze mins
  maxsFrozen <- VS.unsafeFreeze maxs
  let (splitDim, splitWidth) = widestDimension minsFrozen maxsFrozen
  pure (minsFrozen, maxsFrozen, splitDim, splitWidth)

widestDimension :: VS.Vector Int16 -> VS.Vector Int16 -> (Int, Int)
widestDimension mins maxs =
  go 0 0 0
  where
    go !dim !bestDim !bestWidth
      | dim >= dimensionCount = (bestDim, bestWidth)
      | otherwise =
          let width = fromIntegral (maxs VS.! dim) - fromIntegral (mins VS.! dim)
           in if width > bestWidth
                then go (dim + 1) dim width
                else go (dim + 1) bestDim bestWidth

writeMeta :: MVS.IOVector Int32 -> Int -> Int -> Int -> Int -> Int -> IO ()
writeMeta nodeMeta node left right start count = do
  let base = node * nodeMetaFields
  MVS.unsafeWrite nodeMeta base (fromIntegral left)
  MVS.unsafeWrite nodeMeta (base + 1) (fromIntegral right)
  MVS.unsafeWrite nodeMeta (base + 2) (fromIntegral start)
  MVS.unsafeWrite nodeMeta (base + 3) (fromIntegral count)

writeNodeBounds :: MVS.IOVector Int16 -> Int -> VS.Vector Int16 -> VS.Vector Int16 -> IO ()
writeNodeBounds nodeBounds node mins maxs = do
  let base = node * dimensionCount * 2
  forInt 0 dimensionCount $ \dim -> do
    MVS.unsafeWrite nodeBounds (base + dim) (mins VS.! dim)
    MVS.unsafeWrite nodeBounds (base + dimensionCount + dim) (maxs VS.! dim)

selectByDim :: VS.Vector Int16 -> MVS.IOVector Int32 -> Int -> Int -> Int -> Int -> IO ()
selectByDim refs indices left0 right0 kth dim =
  go left0 right0
  where
    go !left !right
      | right - left <= 1 = pure ()
      | otherwise = do
          pivotRow <- fromIntegral <$> MVS.unsafeRead indices (left + (right - left) `div` 2)
          let pivot = refValue refs pivotRow dim
          (lt, gt) <- partition3 left right pivot
          if kth < lt
            then go left lt
            else
              if kth <= gt
                then pure ()
                else go (gt + 1) right

    partition3 !left !right !pivot =
      loop left left (right - 1)
      where
        loop !lt !i !gt
          | i > gt = pure (lt, gt)
          | otherwise = do
              row <- fromIntegral <$> MVS.unsafeRead indices i
              let value = refValue refs row dim
              if value < pivot
                then do
                  swap indices lt i
                  loop (lt + 1) (i + 1) gt
                else
                  if value > pivot
                    then do
                      swap indices i gt
                      loop lt i (gt - 1)
                    else loop lt (i + 1) gt

swap :: MVS.IOVector Int32 -> Int -> Int -> IO ()
swap vec a b =
  if a == b
    then pure ()
    else do
      va <- MVS.unsafeRead vec a
      vb <- MVS.unsafeRead vec b
      MVS.unsafeWrite vec a vb
      MVS.unsafeWrite vec b va

refValue :: VS.Vector Int16 -> Int -> Int -> Int16
refValue refs row dim =
  refs VS.! (row * dimensionCount + dim)

countReferenceVectors :: BS.ByteString -> Int
countReferenceVectors = go 0 0
  where
    needle = "\"vector\""
    go !n !pos bytes =
      case findFrom needle pos bytes of
        Nothing -> n
        Just found -> go (n + 1) (found + BS.length needle) bytes

fillVectors ::
  BS.ByteString ->
  Int ->
  MVS.IOVector Int16 ->
  MVS.IOVector Word8 ->
  IO BuildStats
fillVectors jsonBytes expectedCount vectors labels =
  go 0 0 0 0
  where
    go !row !pos !frauds !legits
      | row >= expectedCount =
          pure
            BuildStats
              { buildStatsReferences = row,
                buildStatsFrauds = frauds,
                buildStatsLegits = legits
              }
      | otherwise =
          case parseReferenceAt jsonBytes pos of
            Left err -> fail err
            Right (parsed, nextPos) -> do
              forM_ (zip [0 ..] (parsedVector parsed)) $ \(dim, value) ->
                MVS.unsafeWrite vectors (row * dimensionCount + dim) value
              MVS.unsafeWrite labels row (parsedLabel parsed)
              let frauds' = frauds + if parsedLabel parsed == 1 then 1 else 0
                  legits' = legits + if parsedLabel parsed == 0 then 1 else 0
              if row > 0 && row `rem` 500000 == 0
                then hPutStrLn stderr ("indexed " <> show row <> " references")
                else pure ()
              go (row + 1) nextPos frauds' legits'

data ParsedReference = ParsedReference
  { parsedVector :: ![Int16],
    parsedLabel :: !Word8
  }

parseReferenceAt :: BS.ByteString -> Int -> Either String (ParsedReference, Int)
parseReferenceAt bytes pos0 = do
  vectorKey <- maybeToEither "vector key not found" (findFrom "\"vector\"" pos0 bytes)
  vectorOpen <- maybeToEither "vector array not found" (findFrom "[" vectorKey bytes)
  (vec, afterVector) <- parseVector bytes (vectorOpen + 1)
  labelKey <- maybeToEither "label key not found" (findFrom "\"label\"" afterVector bytes)
  colon <- maybeToEither "label colon not found" (findFrom ":" labelKey bytes)
  (label, afterLabel) <- parseLabel bytes (colon + 1)
  pure (ParsedReference vec label, afterLabel)

parseVector :: BS.ByteString -> Int -> Either String ([Int16], Int)
parseVector bytes pos0 = do
  (values, posAfterValues) <- parseDims 0 pos0 []
  posClose <- expectByte ']' bytes posAfterValues
  pure (reverse values, posClose + 1)
  where
    parseDims !dim !pos !acc
      | dim >= dimensionCount = pure (acc, pos)
      | otherwise = do
          posValue <- if dim == 0 then pure pos else expectByte ',' bytes pos >>= pure . (+ 1)
          (value, posAfterValue) <- parseNumber bytes posValue
          parseDims (dim + 1) posAfterValue (encodeDimension value : acc)

parseLabel :: BS.ByteString -> Int -> Either String (Word8, Int)
parseLabel bytes pos0 = do
  quote <- expectByte '"' bytes pos0
  let valueStart = quote + 1
      fraud = "fraud"
      legit = "legit"
  if fraud `BS.isPrefixOf` BS.drop valueStart bytes
    then pure (1, valueStart + BS.length fraud + 1)
    else
      if legit `BS.isPrefixOf` BS.drop valueStart bytes
        then pure (0, valueStart + BS.length legit + 1)
        else Left ("unknown label at byte " <> show valueStart)

parseNumber :: BS.ByteString -> Int -> Either String (Double, Int)
parseNumber bytes pos0 =
  let pos = skipSpaces bytes pos0
      slice = BS.drop pos bytes
      (numberBytes, _) = BS.span isNumberByte slice
   in if BS.null numberBytes
        then Left ("number expected at byte " <> show pos)
        else case readMaybe (BSC.unpack numberBytes) of
          Nothing -> Left ("invalid number at byte " <> show pos)
          Just value -> pure (value, pos + BS.length numberBytes)

expectByte :: Char -> BS.ByteString -> Int -> Either String Int
expectByte expected bytes pos0 =
  let pos = skipSpaces bytes pos0
   in if pos < BS.length bytes && BS.index bytes pos == charByte expected
        then Right pos
        else Left ("expected " <> show expected <> " at byte " <> show pos)

skipSpaces :: BS.ByteString -> Int -> Int
skipSpaces bytes = go
  where
    go !pos
      | pos >= BS.length bytes = pos
      | BS.index bytes pos `elem` [32, 9, 10, 13] = go (pos + 1)
      | otherwise = pos

findFrom :: BS.ByteString -> Int -> BS.ByteString -> Maybe Int
findFrom needle pos bytes =
  let haystack = BS.drop pos bytes
      (prefix, suffix) = BS.breakSubstring needle haystack
   in if BS.null suffix
        then Nothing
        else Just (pos + BS.length prefix)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither err = maybe (Left err) Right

charByte :: Char -> Word8
charByte = fromIntegral . fromEnum

isNumberByte :: Word8 -> Bool
isNumberByte byte =
  byte == charByte '-'
    || byte == charByte '+'
    || byte == charByte '.'
    || byte == charByte 'e'
    || byte == charByte 'E'
    || (byte >= charByte '0' && byte <= charByte '9')

forInt :: Int -> Int -> (Int -> IO ()) -> IO ()
forInt start end action = go start
  where
    go !i
      | i >= end = pure ()
      | otherwise = action i >> go (i + 1)

nodeMetaFields :: Int
nodeMetaFields = 4

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = go 1
  where
    go !x
      | x >= n = x
      | otherwise = go (x * 2)
