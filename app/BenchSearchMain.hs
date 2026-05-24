{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Off-the-hot-path benchmark for the exact KD search. Reports throughput and
-- p50/p95/p99 latency over the test-data queries, plus structural counters
-- (nodes visited, lower-bound calls, leaf rows scanned, distance calls) from a
-- separately-instrumented traversal that mirrors the production search. The
-- production search functions stay free of any instrumentation.
module Main (main) where

import qualified Control.Exception as Exception
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (foldl', sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Helheim.Features (EncodedVector)
import Helheim.Index
import Helheim.PackedVector
import Helheim.Types (FraudRequest, FraudResponse (..))
import Helheim.Vectorize (vectorize)
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Vector.Storable as VS

newtype TestData = TestData {entries :: [Entry]}

instance FromJSON TestData where
  parseJSON = withObject "test_data" $ \o -> TestData <$> o .: "entries"

newtype Entry = Entry {entryRequest :: FraudRequest}

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o -> Entry <$> o .: "request"

main :: IO ()
main = do
  args <- getArgs
  let indexPath = optionString "--index" args "data/references.bin"
      testPath = optionString "--test-data" args "rinha-de-backend-2026/test/test-data.json"
      iterations = optionInt "--iterations" args 1
  index <- loadIndex indexPath
  testBytes <- BL.readFile testPath
  testData <- case eitherDecode testBytes of
    Left err -> fail err
    Right value -> pure value
  let queries =
        [ query
          | entry <- entries testData,
            Right query <- [vectorize (entryRequest entry)]
        ]
      queryCount = length queries
  -- Force the query list once so build/vectorize time is excluded from timing.
  _ <- Exception.evaluate (sum (map VS.length queries))

  -- Throughput over `iterations` full passes.
  start <- getMonotonicTimeNSec
  total <- runPasses index queries iterations
  end <- getMonotonicTimeNSec
  let elapsedNs = end - start
      passes = max 1 iterations
      totalSearches = queryCount * passes
      throughput = fromIntegral totalSearches / (fromIntegral elapsedNs / 1.0e9) :: Double

  -- Per-query latencies from one pass.
  latencies <- mapM (timeOne index) queries
  let sorted = sort latencies

  -- Structural counters from the instrumented mirror (one pass).
  let stats = foldl' (\ !acc q -> addStats acc (walk index q)) emptyStats queries
  _ <- Exception.evaluate (sNodes stats + sLowerBound stats + sLeafRows stats + sDistance stats)

  printf "queries:           %d\n" queryCount
  printf "iterations:        %d\n" passes
  printf "total searches:    %d\n" totalSearches
  printf "checksum:          %d\n" total
  printf "throughput:        %.0f searches/s\n" throughput
  printf "latency p50_ms:    %.6f\n" (percentileMs 0.50 sorted)
  printf "latency p95_ms:    %.6f\n" (percentileMs 0.95 sorted)
  printf "latency p99_ms:    %.6f\n" (percentileMs 0.99 sorted)
  printf "nodes visited:     %d (%.2f/query)\n" (sNodes stats) (perQuery (sNodes stats) queryCount)
  printf "lower-bound calls: %d (%.2f/query)\n" (sLowerBound stats) (perQuery (sLowerBound stats) queryCount)
  printf "leaf rows scanned: %d (%.2f/query)\n" (sLeafRows stats) (perQuery (sLeafRows stats) queryCount)
  printf "distance calls:    %d (%.2f/query)\n" (sDistance stats) (perQuery (sDistance stats) queryCount)

perQuery :: Int -> Int -> Double
perQuery _ 0 = 0
perQuery n q = fromIntegral n / fromIntegral q

-- | Run `iterations` passes, summing a checksum derived from each score so the
-- searches cannot be optimized away.
runPasses :: ReferenceIndex -> [EncodedVector] -> Int -> IO Int
runPasses index queries iterations = go 0 (max 1 iterations)
  where
    go !acc 0 = pure acc
    go !acc n = do
      s <- Exception.evaluate (foldl' (\ !a q -> a + scoreKey index q) 0 queries)
      go (acc + s) (n - 1)

scoreKey :: ReferenceIndex -> EncodedVector -> Int
scoreKey index q = round (fraudResponseScore (searchIndex index q) * 1000)

timeOne :: ReferenceIndex -> EncodedVector -> IO Word64
timeOne index q = do
  t0 <- getMonotonicTimeNSec
  _ <- Exception.evaluate (fraudResponseScore (searchIndex index q))
  t1 <- getMonotonicTimeNSec
  pure (t1 - t0)

percentileMs :: Double -> [Word64] -> Double
percentileMs _ [] = 0
percentileMs p xs = fromIntegral (xs !! idx) / 1.0e6
  where
    len = length xs
    idx = min (len - 1) (max 0 (ceiling (p * fromIntegral len) - 1))

-- Instrumented traversal --------------------------------------------------

data Stats = Stats
  { sNodes :: !Int,
    sLowerBound :: !Int,
    sLeafRows :: !Int,
    sDistance :: !Int
  }

emptyStats :: Stats
emptyStats = Stats 0 0 0 0

addStats :: Stats -> Stats -> Stats
addStats a b =
  Stats
    (sNodes a + sNodes b)
    (sLowerBound a + sLowerBound b)
    (sLeafRows a + sLeafRows b)
    (sDistance a + sDistance b)

-- | Mirrors 'kdFrauds' (cutoffs + threaded child bounds) but tracks only the
-- 5 best distances (enough to drive pruning) and accumulates work counters.
walk :: ReferenceIndex -> EncodedVector -> Stats
walk index query = snd (go 0 0 ([], emptyStats))
  where
    nodes = referenceNodeCount index
    go !node !nodeLB acc@(best, st)
      | node < 0 || node >= nodes = acc
      | nodeLB >= worstD best = acc
      | nLeft node < 0 =
          scanLeaf (nStart node) (nCount node) (best, st {sNodes = sNodes st + 1})
      | otherwise =
          let st1 = st {sNodes = sNodes st + 1, sLowerBound = sLowerBound st + 2}
              left = nLeft node
              right = nRight node
              cutoff = worstD best
              lbL = packedLowerBoundUnder (referenceNodeBoundWords index) query left cutoff
              lbR = packedLowerBoundUnder (referenceNodeBoundWords index) query right cutoff
              (f, fb, s, sb) =
                if lbL <= lbR then (left, lbL, right, lbR) else (right, lbR, left, lbL)
              accFirst@(bestF, _) =
                if fb < worstD best then go f fb (best, st1) else (best, st1)
           in if sb < worstD bestF then go s sb accFirst else accFirst

    scanLeaf !i !cnt acc = leafLoop i (i + cnt) acc
    leafLoop !i !end acc@(best, st)
      | i >= end = acc
      | otherwise =
          let cutoff = worstD best
              d = packedSquaredDistanceUnder (referenceVectorWords index) query i cutoff
              best' = insertD d best
              st' = st {sLeafRows = sLeafRows st + 1, sDistance = sDistance st + 1}
           in leafLoop (i + 1) end (best', st')

    nLeft, nRight, nStart, nCount :: Int -> Int
    nLeft n = fromIntegral (referenceNodeMeta index VS.! (n * 4))
    nRight n = fromIntegral (referenceNodeMeta index VS.! (n * 4 + 1))
    nStart n = fromIntegral (referenceNodeMeta index VS.! (n * 4 + 2))
    nCount n = fromIntegral (referenceNodeMeta index VS.! (n * 4 + 3))

-- A length<=5 ascending list of the best distances seen.
insertD :: Int64 -> [Int64] -> [Int64]
insertD d = take 5 . ins
  where
    ins [] = [d]
    ins (x : xs)
      | d <= x = d : x : xs
      | otherwise = x : ins xs

worstD :: [Int64] -> Int64
worstD bs
  | length bs < 5 = maxBound
  | otherwise = last bs

optionString :: String -> [String] -> String -> String
optionString name args fallback = maybe fallback id (lookupArg name args)

optionInt :: String -> [String] -> Int -> Int
optionInt name args fallback =
  case lookupArg name args >>= parse of
    Just n -> n
    Nothing -> fallback
  where
    parse s = case reads s :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing

lookupArg :: String -> [String] -> Maybe String
lookupArg name args =
  case dropWhile (/= name) args of
    (_ : value : _) -> Just value
    _ -> Nothing
