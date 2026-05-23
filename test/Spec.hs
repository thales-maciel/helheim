{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Helheim.Index
import Helheim.ReferenceBuilder
import Helheim.Types
import Helheim.Vectorize
import qualified Data.Vector.Storable as VS
import System.Exit (exitFailure)

main :: IO ()
main = do
  testVectorizeNullLastTransaction
  testVectorizeKnownMerchant
  testMccDefault
  testIndexSearch
  testKdIndexBuild
  testReferenceParser
  putStrLn "helheim-test passed"

testVectorizeNullLastTransaction :: IO ()
testVectorizeNullLastTransaction = do
  request <- decodeRequest firstExamplePayload
  let expected =
        [ 41,
          1667,
          500,
          7826,
          3333,
          -10000,
          -10000,
          292,
          1500,
          0,
          10000,
          0,
          1500,
          60
        ]
  assertEqual "vectorize first example" expected (encoded request)

testVectorizeKnownMerchant :: IO ()
testVectorizeKnownMerchant = do
  request <- decodeRequest secondExamplePayload
  let values = encoded request
  assertEqual "known merchant flag" 0 (values !! 11)
  assertEqual "mcc risk" 2000 (values !! 12)

testMccDefault :: IO ()
testMccDefault =
  assertEqual "default MCC risk" 0.5 (mccRisk "0000")

testIndexSearch :: IO ()
testIndexSearch = do
  let query = VS.fromList (replicate 14 0)
      legit = VS.fromList (replicate 14 0)
      fraud = VS.fromList (replicate 14 10000)
      index =
        ReferenceIndex
          { referenceCount = 5,
            referenceVectors = VS.concat [legit, legit, legit, fraud, fraud],
            referenceLabels = VS.fromList [0, 0, 0, 1, 1],
            referenceNodeCount = 0,
            referenceNodeMeta = VS.empty,
            referenceNodeBounds = VS.empty
          }
      response = searchIndex index query
  assertEqual "approved result" True (fraudResponseApproved response)
  assertEqual "fraud score" 0.4 (fraudResponseScore response)

testKdIndexBuild :: IO ()
testKdIndexBuild = do
  bytes <- BL.readFile "rinha-de-backend-2026/resources/example-references.json"
  _ <- buildIndexFromJsonBytes "/tmp/helheim-test-index.bin" (BL.toStrict bytes)
  index <- loadIndex "/tmp/helheim-test-index.bin"
  let query =
        VS.fromList
          [ 100,
            833,
            500,
            8261,
            1667,
            -10000,
            -10000,
            432,
            2500,
            0,
            10000,
            0,
            2000,
            416
          ]
      response = searchIndex index query
  assertEqual "kd reference count" 100 (referenceCount index)
  assertEqual "kd nodes built" True (referenceNodeCount index > 0)
  assertEqual "kd approved result" True (fraudResponseApproved response)

testReferenceParser :: IO ()
testReferenceParser = do
  bytes <- BL.readFile "rinha-de-backend-2026/resources/example-references.json"
  let strictBytes = BL.toStrict bytes
  assertEqual "reference count" 100 (countReferenceVectors strictBytes)

decodeRequest :: BL.ByteString -> IO FraudRequest
decodeRequest bytes =
  case eitherDecode bytes of
    Left err -> fail err
    Right value -> pure value

encoded :: FraudRequest -> [Int16]
encoded request =
  case vectorize request of
    Left err -> error err
    Right value -> toEncodedList value

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else do
      putStrLn (label <> " failed")
      putStrLn ("expected: " <> show expected)
      putStrLn ("actual: " <> show actual)
      exitFailure

firstExamplePayload :: BL.ByteString
firstExamplePayload =
  "{\"id\":\"tx-1329056812\",\"transaction\":{\"amount\":41.12,\"installments\":2,\"requested_at\":\"2026-03-11T18:45:53Z\"},\"customer\":{\"avg_amount\":82.24,\"tx_count_24h\":3,\"known_merchants\":[\"MERC-003\",\"MERC-016\"]},\"merchant\":{\"id\":\"MERC-016\",\"mcc\":\"5411\",\"avg_amount\":60.25},\"terminal\":{\"is_online\":false,\"card_present\":true,\"km_from_home\":29.2331036248},\"last_transaction\":null}"

secondExamplePayload :: BL.ByteString
secondExamplePayload =
  "{\"id\":\"tx-3576980410\",\"transaction\":{\"amount\":384.88,\"installments\":3,\"requested_at\":\"2026-03-11T20:23:35Z\"},\"customer\":{\"avg_amount\":769.76,\"tx_count_24h\":3,\"known_merchants\":[\"MERC-009\",\"MERC-001\",\"MERC-001\"]},\"merchant\":{\"id\":\"MERC-001\",\"mcc\":\"5912\",\"avg_amount\":298.95},\"terminal\":{\"is_online\":false,\"card_present\":true,\"km_from_home\":13.7090520965},\"last_transaction\":{\"timestamp\":\"2026-03-11T14:58:35Z\",\"km_from_current\":18.8626479774}}"
