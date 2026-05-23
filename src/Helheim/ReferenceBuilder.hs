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
import Data.Int (Int16)
import Data.Word (Word8)
import Helheim.Index
import Helheim.Vectorize (encodeDimension)
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

buildIndexFromGzip :: FilePath -> FilePath -> IO BuildStats
buildIndexFromGzip input output = do
  compressed <- BL.readFile input
  let jsonBytes = BL.toStrict (GZip.decompress compressed)
  buildIndexFromJsonBytes output jsonBytes

buildIndexFromJsonBytes :: FilePath -> BS.ByteString -> IO BuildStats
buildIndexFromJsonBytes output jsonBytes = do
  let count = countReferenceVectors jsonBytes
  hPutStrLn stderr ("building reference index for " <> show count <> " vectors")
  vectors <- MVS.new (count * dimensions)
  labels <- MVS.new count
  stats <- fillVectors jsonBytes count vectors labels
  frozenVectors <- VS.unsafeFreeze vectors
  frozenLabels <- VS.unsafeFreeze labels
  saveIndex
    output
    ReferenceIndex
      { referenceCount = count,
        referenceVectors = frozenVectors,
        referenceLabels = frozenLabels
      }
  pure stats

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
                MVS.unsafeWrite vectors (row * dimensions + dim) value
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
      | dim >= dimensions = pure (acc, pos)
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

dimensions :: Int
dimensions = 14
