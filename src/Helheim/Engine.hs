{-# LANGUAGE DerivingStrategies #-}

module Helheim.Engine
  ( Engine (..),
    EngineMode (..),
    classify,
    classifyCount,
    engineModeFromString,
  )
where

import Helheim.Features
import Helheim.Index
import Helheim.Types

data EngineMode = ExactMode | HybridMode
  deriving stock (Eq, Show)

data Engine = Engine
  { engineMode :: !EngineMode,
    engineIndex :: !ReferenceIndex
  }

engineModeFromString :: String -> EngineMode
engineModeFromString "exact" = ExactMode
engineModeFromString "Exact" = ExactMode
engineModeFromString "EXACT" = ExactMode
engineModeFromString "hybrid" = HybridMode
engineModeFromString "Hybrid" = HybridMode
engineModeFromString "HYBRID" = HybridMode
engineModeFromString _ = ExactMode

classify :: Engine -> FraudRequest -> EncodedVector -> FraudResponse
classify engine request query =
  case engineMode engine of
    ExactMode -> searchIndex (engineIndex engine) query
    HybridMode ->
      case shortcut request (encodedFeatures query) of
        Just response -> response
        Nothing -> searchIndex (engineIndex engine) query

-- | Like 'classify', but returns just the fraud count in [0,5] so the API can
-- index directly into pre-baked responses without building a 'FraudResponse'
-- or round-tripping through the Double score.
classifyCount :: Engine -> FraudRequest -> EncodedVector -> Int
classifyCount engine request query =
  case engineMode engine of
    ExactMode -> fraudCount (engineIndex engine) query
    HybridMode ->
      case shortcut request (encodedFeatures query) of
        Just response -> scoreToCount (fraudResponseScore response)
        Nothing -> fraudCount (engineIndex engine) query

-- A shortcut response carries a 0.0 (clear legit) or 1.0 (clear fraud) score;
-- map it back to the equivalent neighbour count for the pre-baked response table.
scoreToCount :: Double -> Int
scoreToCount s = max 0 (min 5 (round (s * 5)))

shortcut :: FraudRequest -> EncodedFeatures -> Maybe FraudResponse
shortcut request features
  | clearLegit request features = Just (FraudResponse True 0.0)
  | clearFraud features = Just (FraudResponse False 1.0)
  | otherwise = Nothing

clearLegit :: FraudRequest -> EncodedFeatures -> Bool
clearLegit request features =
  encodedAmount features <= clearLegitMaxAmount
    && encodedInstallments features <= clearLegitMaxInstallments
    && encodedAmountVsAverage features <= clearLegitMaxAmountVsAverage
    && encodedKmFromHome features <= clearLegitMaxKmFromHome
    && encodedTxCount24h features <= clearLegitMaxTxCount24h
    && encodedUnknownMerchant features == knownMerchant
    && encodedMccRisk features <= clearLegitMaxMccRisk
    && isBusinessHour (encodedHour features)
    && lastLooksLegit
  where
    lastLooksLegit =
      case fraudRequestLastTransaction request of
        Nothing -> True
        Just _ -> encodedKmFromLast features <= clearLegitMaxKmFromLast

clearFraud :: EncodedFeatures -> Bool
clearFraud features =
  encodedAmount features >= clearFraudMinAmount
    && encodedInstallments features >= clearFraudMinInstallments
    && encodedAmountVsAverage features >= clearFraudMinAmountVsAverage
    && encodedHour features <= clearFraudLatestHour
    && encodedKmFromHome features >= clearFraudMinKmFromHome
    && encodedTxCount24h features >= clearFraudMinTxCount24h
    && encodedUnknownMerchant features == unknownMerchant
    && encodedMccRisk features >= clearFraudMinMccRisk
    && (encodedMinutesSinceLast features == missingFeature || encodedKmFromLast features >= clearFraudMinKmFromLast)

isBusinessHour :: EncodedFeature -> Bool
isBusinessHour hour =
  between hour businessHourStart businessHourEnd

between :: Ord a => a -> a -> a -> Bool
between value low high =
  value >= low && value <= high

knownMerchant :: EncodedFeature
knownMerchant = flagAt (FeatureFlag False)

unknownMerchant :: EncodedFeature
unknownMerchant = flagAt (FeatureFlag True)

clearLegitMaxAmount :: EncodedFeature
clearLegitMaxAmount = amountAt (Amount 600)

clearLegitMaxInstallments :: EncodedFeature
clearLegitMaxInstallments = installmentsAt (Installments 4)

clearLegitMaxAmountVsAverage :: EncodedFeature
clearLegitMaxAmountVsAverage = amountVsAverageAt (AmountVsAverage 0.6)

clearLegitMaxKmFromHome :: EncodedFeature
clearLegitMaxKmFromHome = kilometersAt (Kilometers 60)

clearLegitMaxTxCount24h :: EncodedFeature
clearLegitMaxTxCount24h = txCount24hAt (TxCount24h 5)

clearLegitMaxMccRisk :: EncodedFeature
clearLegitMaxMccRisk = mccRiskAt (MccRiskScore 0.30)

businessHourStart :: EncodedFeature
businessHourStart = hourAt (HourOfDay 7)

businessHourEnd :: EncodedFeature
businessHourEnd = hourAt (HourOfDay 20)

clearLegitMaxKmFromLast :: EncodedFeature
clearLegitMaxKmFromLast = kilometersAt (Kilometers 40)

clearFraudMinAmount :: EncodedFeature
clearFraudMinAmount = amountAt (Amount 3000)

clearFraudMinInstallments :: EncodedFeature
clearFraudMinInstallments = installmentsAt (Installments 6)

clearFraudMinAmountVsAverage :: EncodedFeature
clearFraudMinAmountVsAverage = amountVsAverageAt (AmountVsAverage 8)

clearFraudLatestHour :: EncodedFeature
clearFraudLatestHour = hourAt (HourOfDay 6)

clearFraudMinKmFromHome :: EncodedFeature
clearFraudMinKmFromHome = kilometersAt (Kilometers 300)

clearFraudMinTxCount24h :: EncodedFeature
clearFraudMinTxCount24h = txCount24hAt (TxCount24h 8)

clearFraudMinMccRisk :: EncodedFeature
clearFraudMinMccRisk = mccRiskAt (MccRiskScore 0.75)

clearFraudMinKmFromLast :: EncodedFeature
clearFraudMinKmFromLast = kilometersAt (Kilometers 200)
