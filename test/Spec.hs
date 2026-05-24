{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int64)
import Data.Maybe (isJust, isNothing)
import Helheim.Index
import Helheim.PackedVector
import Helheim.ReferenceBuilder
import Helheim.RequestParser (parseFraudRequest, runFast)
import Helheim.Types
import Helheim.Vectorize
import qualified Data.Vector.Storable as VS
import System.Exit (exitFailure)

main :: IO ()
main = do
  testVectorizeNullLastTransaction
  testVectorizeKnownMerchant
  testMccDefault
  testPackedRoundTrip
  testPackedLabels
  testPackedDistance
  testDecodeDimParity
  testSpecializedDistanceParity
  testSpecializedLowerBoundParity
  testCutoffCorrectness
  testIndexSearch
  testKdIndexBuild
  testSearchResultParity
  testRequestParserParity
  testRequestParserFallback
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

testPackedRoundTrip :: IO ()
testPackedRoundTrip = do
  let values =
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
      packed = expectRight (packEncodedList values)
      unpacked = [packedDimensionAtIndex packed dim | dim <- [0 .. dimensionCount - 1]]
      unpackedFast = [decodeDimAt packed dim | dim <- [0 .. dimensionCount - 1]]
  assertEqual "packed vector roundtrip" values unpacked
  assertEqual "packed vector roundtrip (decodeDimAt)" values unpackedFast

testPackedLabels :: IO ()
testPackedLabels = do
  let labels = VS.fromList (1 : replicate 62 0 <> [1, 1])
      packed = expectRight (packLabels 65 labels)
  assertEqual "first packed label" 1 (labelAt packed 0)
  assertEqual "middle packed label" 0 (labelAt packed 20)
  assertEqual "word boundary packed label" 1 (labelAt packed 63)
  assertEqual "second word packed label" 1 (labelAt packed 64)

testPackedDistance :: IO ()
testPackedDistance = do
  let query = validLegitVector
      reference = validFraudVector
      packedRefs = expectRight (packReferenceVectors 1 reference)
  assertEqual "packed squared distance" (plainDistance query reference) (packedSquaredDistance packedRefs query 0)

-- | The specialized 'decodeDimAt' must equal the generic 'packedDimensionAtIndex'
-- for every dimension across vectors that exercise all codecs (generic,
-- maybe-missing, and every finite codebook including the non-monotone MCC table).
testDecodeDimParity :: IO ()
testDecodeDimParity =
  mapM_ checkVector decodeParitySamples
  where
    checkVector vec =
      let packed = expectRight (packEncodedVector vec)
       in mapM_ (checkDim packed) [0 .. dimensionCount - 1]
    checkDim packed dim =
      assertEqual
        ("decodeDimAt parity dim " <> show dim)
        (packedDimensionAtIndex packed dim)
        (decodeDimAt packed dim)

decodeParitySamples :: [VS.Vector Int16]
decodeParitySamples =
  [ validLegitVector,
    validFraudVector,
    -- missing minutes/km, MCC index 0, mixed flags
    VS.fromList [41, 1667, 500, 7826, 3333, -10000, -10000, 292, 1500, 0, 10000, 0, 1500, 60],
    -- MCC = 8500 (codebook index 6, non-monotone), present last-tx
    VS.fromList [10000, 833, 10000, 8261, 1667, 5000, 200, 432, 2500, 10000, 0, 10000, 8500, 416]
  ]

-- | The unrolled 'packedSquaredDistance' must equal both the model distance
-- and the retained generic oracle 'packedSquaredDistanceRef'.
testSpecializedDistanceParity :: IO ()
testSpecializedDistanceParity =
  mapM_ check [(validLegitVector, validFraudVector), (validFraudVector, validLegitVector), (validLegitVector, validLegitVector)]
  where
    check (query, reference) =
      let packedRefs = expectRight (packReferenceVectors 1 reference)
       in do
            assertEqual "specialized distance vs model" (plainDistance query reference) (packedSquaredDistance packedRefs query 0)
            assertEqual "specialized distance vs oracle" (packedSquaredDistanceRef packedRefs query 0) (packedSquaredDistance packedRefs query 0)

-- | The unrolled 'packedLowerBound' must equal the generic oracle for queries
-- inside, below, and above the node bounding box (all three clamp branches).
testSpecializedLowerBoundParity :: IO ()
testSpecializedLowerBoundParity =
  mapM_ check queries
  where
    bounds = expectRight (packNodeBounds 1 (validLegitVector VS.++ validFraudVector))
    queries =
      [ validLegitVector,
        validFraudVector,
        VS.replicate dimensionCount (-5000),
        VS.replicate dimensionCount 15000,
        VS.fromList [5000, 1667, 5000, 4348, 1667, 0, 0, 5000, 5000, 0, 10000, 0, 5000, 5000]
      ]
    check q =
      assertEqual "specialized lower bound vs oracle" (packedLowerBoundRef bounds q 0) (packedLowerBound bounds q 0)

-- | Cutoff variants: identical to the exact value when the true value is below
-- the cutoff (including the maxBound seed); otherwise a value >= cutoff.
testCutoffCorrectness :: IO ()
testCutoffCorrectness = do
  let query = validLegitVector
      reference = validFraudVector
      packedRefs = expectRight (packReferenceVectors 1 reference)
      v = packedSquaredDistance packedRefs query 0
  assertEqual "cutoff distance exact under maxBound" v (packedSquaredDistanceUnder packedRefs query 0 maxBoundI64)
  assertEqual "cutoff distance exact under v+1" v (packedSquaredDistanceUnder packedRefs query 0 (v + 1))
  assertEqual "cutoff distance >= cutoff at v" True (packedSquaredDistanceUnder packedRefs query 0 v >= v)
  assertEqual "cutoff distance >= cutoff at 1" True (packedSquaredDistanceUnder packedRefs query 0 1 >= 1)
  let bounds = expectRight (packNodeBounds 1 (validLegitVector VS.++ validFraudVector))
      lbQuery = VS.replicate dimensionCount 15000
      lbV = packedLowerBound bounds lbQuery 0
  assertEqual "cutoff lower bound positive" True (lbV > 0)
  assertEqual "cutoff lower bound exact under maxBound" lbV (packedLowerBoundUnder bounds lbQuery 0 maxBoundI64)
  assertEqual "cutoff lower bound exact under lbV+1" lbV (packedLowerBoundUnder bounds lbQuery 0 (lbV + 1))
  assertEqual "cutoff lower bound >= cutoff at lbV" True (packedLowerBoundUnder bounds lbQuery 0 lbV >= lbV)
  assertEqual "cutoff lower bound >= cutoff at 1" True (packedLowerBoundUnder bounds lbQuery 0 1 >= 1)
  where
    maxBoundI64 = maxBound :: Int64

testIndexSearch :: IO ()
testIndexSearch = do
  let query = validLegitVector
      legit = validLegitVector
      fraud = validFraudVector
      vectors = VS.concat [legit, legit, legit, fraud, fraud]
      labels = VS.fromList [0, 0, 0, 1, 1]
      index =
        ReferenceIndex
          { referenceCount = 5,
            referenceVectorWords = expectRight (packReferenceVectors 5 vectors),
            referenceLabelWords = expectRight (packLabels 5 labels),
            referenceNodeCount = 0,
            referenceNodeMeta = VS.empty,
            referenceNodeBoundWords = expectRight (packNodeBounds 0 VS.empty)
          }
      response = searchIndex index query
  assertEqual "approved result" True (fraudResponseApproved response)
  assertEqual "fraud score" 0.4 (fraudResponseScore response)

testKdIndexBuild :: IO ()
testKdIndexBuild = do
  bytes <- BL.readFile "rinha-de-backend-2026/resources/example-references.json"
  _ <- buildIndexFromJsonBytes "/tmp/helheim-test-index.bin" 64 (BL.toStrict bytes)
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

-- | End-to-end parity: the KD search (with cutoffs + threaded bounds) must
-- produce the same fraud score as the independent flat brute-force scan over
-- the same references. Forcing referenceNodeCount = 0 routes 'fraudScore'
-- through 'flatFrauds', a code path that shares no traversal logic with the KD
-- search, so a mismatch flags any KD/cutoff divergence.
testSearchResultParity :: IO ()
testSearchResultParity = do
  bytes <- BL.readFile "rinha-de-backend-2026/resources/example-references.json"
  _ <- buildIndexFromJsonBytes "/tmp/helheim-parity-index.bin" 64 (BL.toStrict bytes)
  index <- loadIndex "/tmp/helheim-parity-index.bin"
  let flat = index {referenceNodeCount = 0}
      queries =
        [ validLegitVector,
          validFraudVector,
          VS.fromList [100, 833, 500, 8261, 1667, -10000, -10000, 432, 2500, 0, 10000, 0, 2000, 416],
          VS.fromList [5000, 833, 5000, 4348, 5000, 5000, 200, 200, 5000, 10000, 0, 0, 4500, 5000]
        ]
  mapM_ (check index flat) queries
  where
    check kd flat q =
      assertEqual
        "kd vs flat score parity"
        (fraudResponseScore (searchIndex flat q))
        (fraudResponseScore (searchIndex kd q))

-- | The fast byte-level request parser must produce results identical to the
-- Aeson oracle on real wire payloads, and must actually take the fast path
-- (not silently fall back).
testRequestParserParity :: IO ()
testRequestParserParity = mapM_ check requestParityPayloads
  where
    check payload = do
      let strict = BL.toStrict payload
          fast = parseFraudRequest strict
          oracle = eitherDecode payload :: Either String FraudRequest
      assertEqual "request parser fast path taken" True (isJust (runFast strict))
      case (fast, oracle) of
        (Right fr, Right orq) -> do
          assertEqual "request parser FraudRequest parity" orq fr
          assertEqual "request parser vectorize parity" (vectorize orq) (vectorize fr)
        _ -> do
          putStrLn "request parser parity: unexpected decode failure"
          exitFailure

-- | A string escape makes the fast path bail; the public parser must still
-- match Aeson via the fallback.
testRequestParserFallback :: IO ()
testRequestParserFallback = do
  let strict = BL.toStrict escapedIdPayload
  assertEqual "escaped string bails fast path" True (isNothing (runFast strict))
  assertEqual
    "fallback vectorize parity"
    (vectorize <$> (eitherDecode escapedIdPayload :: Either String FraudRequest))
    (vectorize <$> parseFraudRequest strict)

requestParityPayloads :: [BL.ByteString]
requestParityPayloads =
  [ firstExamplePayload,
    secondExamplePayload,
    -- present last_transaction, empty known_merchants, high-risk mcc, online terminal
    "{\"id\":\"tx-99\",\"transaction\":{\"amount\":1234.5,\"installments\":6,\"requested_at\":\"2027-01-02T03:04:05Z\"},\"customer\":{\"avg_amount\":100.0,\"tx_count_24h\":15,\"known_merchants\":[]},\"merchant\":{\"id\":\"MERC-777\",\"mcc\":\"7995\",\"avg_amount\":4200.99},\"terminal\":{\"is_online\":true,\"card_present\":false,\"km_from_home\":812.5},\"last_transaction\":{\"timestamp\":\"2027-01-02T01:00:00Z\",\"km_from_current\":42.123456789}}",
    -- unknown mcc (default risk), single known merchant equal to merchant id
    "{\"id\":\"tx-100\",\"transaction\":{\"amount\":0.0,\"installments\":0,\"requested_at\":\"2026-12-31T23:59:59Z\"},\"customer\":{\"avg_amount\":50.0,\"tx_count_24h\":1,\"known_merchants\":[\"MERC-001\"]},\"merchant\":{\"id\":\"MERC-001\",\"mcc\":\"0000\",\"avg_amount\":10.0},\"terminal\":{\"is_online\":false,\"card_present\":true,\"km_from_home\":0.0},\"last_transaction\":null}"
  ]

-- firstExamplePayload with an escaped slash in the (unused) id field; valid
-- JSON that the fast path declines, forcing the Aeson fallback.
escapedIdPayload :: BL.ByteString
escapedIdPayload =
  "{\"id\":\"tx-\\/1329056812\",\"transaction\":{\"amount\":41.12,\"installments\":2,\"requested_at\":\"2026-03-11T18:45:53Z\"},\"customer\":{\"avg_amount\":82.24,\"tx_count_24h\":3,\"known_merchants\":[\"MERC-003\",\"MERC-016\"]},\"merchant\":{\"id\":\"MERC-016\",\"mcc\":\"5411\",\"avg_amount\":60.25},\"terminal\":{\"is_online\":false,\"card_present\":true,\"km_from_home\":29.2331036248},\"last_transaction\":null}"

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

expectRight :: (Show e) => Either e a -> a
expectRight result =
  case result of
    Left err -> error (show err)
    Right value -> value

validLegitVector :: VS.Vector Int16
validLegitVector =
  VS.fromList
    [ 0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      1500,
      0
    ]

validFraudVector :: VS.Vector Int16
validFraudVector =
  VS.fromList
    [ 10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      10000,
      8500,
      10000
    ]

plainDistance :: VS.Vector Int16 -> VS.Vector Int16 -> Int64
plainDistance left right =
  go 0 0
  where
    go !dim !acc
      | dim >= dimensionCount = acc
      | otherwise =
          let a = fromIntegral (left VS.! dim) :: Int64
              b = fromIntegral (right VS.! dim) :: Int64
              diff = a - b
           in go (dim + 1) (acc + diff * diff)

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
