{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Helheim.Engine
import Helheim.Index
import Helheim.Types
import Helheim.Vectorize
import System.Environment (getArgs)
import qualified Control.Exception as Exception

data TestData = TestData
  { entries :: ![Entry]
  }
  deriving stock (Generic, Show)

instance FromJSON TestData where
  parseJSON = withObject "test_data" $ \o ->
    TestData <$> o .: "entries"

data Entry = Entry
  { entryRequest :: !FraudRequest,
    entryExpectedApproved :: !Bool
  }
  deriving stock (Generic, Show)

instance FromJSON Entry where
  parseJSON = withObject "entry" $ \o ->
    Entry
      <$> o .: "request"
      <*> o .: "expected_approved"

data Counts = Counts
  { truePositive :: !Int,
    trueNegative :: !Int,
    falsePositive :: !Int,
    falseNegative :: !Int,
    errors :: !Int,
    latenciesNs :: ![Word64]
  }

main :: IO ()
main = do
  args <- getArgs
  let indexPath = optionString "--index" args "data/references.bin"
      testPath = optionString "--test-data" args "rinha-de-backend-2026/test/test-data.json"
      mode = engineModeFromString (optionString "--engine" args "hybrid")
  index <- loadIndex indexPath
  testBytes <- BL.readFile testPath
  testData <-
    case eitherDecode testBytes of
      Left err -> fail err
      Right value -> pure value
  counts <- evaluateEntries Engine {engineMode = mode, engineIndex = index} (entries testData)
  BLC.putStrLn (encode (summary mode counts))

evaluateEntries :: Engine -> [Entry] -> IO Counts
evaluateEntries engine =
  foldlM step (Counts 0 0 0 0 0 [])
  where
    step counts entry = do
      started <- getMonotonicTimeNSec
      let result = do
            query <- vectorize (entryRequest entry)
            pure (classify engine (entryRequest entry) query)
      forced <- Exception.evaluate (forceResult result)
      finished <- getMonotonicTimeNSec
      let latency = finished - started
      pure (record counts latency (entryExpectedApproved entry) forced)

forceResult :: Either String FraudResponse -> Either String FraudResponse
forceResult result =
  case result of
    Left err -> length err `seq` result
    Right response ->
      fraudResponseApproved response `seq`
        fraudResponseScore response `seq`
          result

record :: Counts -> Word64 -> Bool -> Either String FraudResponse -> Counts
record counts latency expectedApproved result =
  case result of
    Left _ ->
      counts {errors = errors counts + 1, latenciesNs = latency : latenciesNs counts}
    Right response
      | fraudResponseApproved response == expectedApproved && expectedApproved ->
          counts {trueNegative = trueNegative counts + 1, latenciesNs = latency : latenciesNs counts}
      | fraudResponseApproved response == expectedApproved ->
          counts {truePositive = truePositive counts + 1, latenciesNs = latency : latenciesNs counts}
      | fraudResponseApproved response ->
          counts {falseNegative = falseNegative counts + 1, latenciesNs = latency : latenciesNs counts}
      | otherwise ->
          counts {falsePositive = falsePositive counts + 1, latenciesNs = latency : latenciesNs counts}

summary :: EngineMode -> Counts -> Value
summary mode counts =
  object
    [ "engine" .= show mode,
      "total" .= total,
      "breakdown"
        .= object
          [ "true_positive_detections" .= truePositive counts,
            "true_negative_detections" .= trueNegative counts,
            "false_positive_detections" .= falsePositive counts,
            "false_negative_detections" .= falseNegative counts,
            "errors" .= errors counts
          ],
      "weighted_errors_E" .= weightedErrors,
      "failure_rate" .= failureRate,
      "latency"
        .= object
          [ "p50_ms" .= percentileMs 0.50 sortedLatencies,
            "p95_ms" .= percentileMs 0.95 sortedLatencies,
            "p99_ms" .= percentileMs 0.99 sortedLatencies
          ]
    ]
  where
    total =
      truePositive counts
        + trueNegative counts
        + falsePositive counts
        + falseNegative counts
        + errors counts
    weightedErrors =
      falsePositive counts
        + 3 * falseNegative counts
        + 5 * errors counts
    failures = falsePositive counts + falseNegative counts + errors counts
    failureRate =
      if total == 0
        then 0 :: Double
        else fromIntegral failures / fromIntegral total
    sortedLatencies = sort (latenciesNs counts)

percentileMs :: Double -> [Word64] -> Double
percentileMs _ [] = 0
percentileMs p xs =
  fromIntegral (xs !! index) / 1000000
  where
    len = length xs
    index = min (len - 1) (max 0 (ceiling (p * fromIntegral len) - 1))

optionString :: String -> [String] -> String -> String
optionString name args fallback =
  maybe fallback id (lookupArg name args)

lookupArg :: String -> [String] -> Maybe String
lookupArg name args =
  case dropWhile (/= name) args of
    (_ : value : _) -> Just value
    _ -> Nothing

foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ acc [] = pure acc
foldlM f acc (x : xs) = f acc x >>= \acc' -> foldlM f acc' xs
