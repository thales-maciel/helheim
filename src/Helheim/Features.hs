{-# LANGUAGE DerivingStrategies #-}

module Helheim.Features
  ( Amount (..),
    AmountVsAverage (..),
    Dimension (..),
    EncodedFeature,
    EncodedFeatures (..),
    EncodedVector,
    FeatureFlag (..),
    FraudFeatures (..),
    HourOfDay (..),
    Installments (..),
    Kilometers (..),
    MccRiskScore (..),
    MerchantAverageAmount (..),
    Minutes (..),
    TxCount24h (..),
    WeekdayMondayZero (..),
    allDimensions,
    amountAt,
    amountVsAverageAt,
    dimensionCount,
    dimensionIndex,
    encodeDimension,
    encodeFraudFeatures,
    encodedFeatureValue,
    encodedFeatures,
    encodedAt,
    flagAt,
    hourAt,
    installmentsAt,
    kilometersAt,
    mccRiskAt,
    merchantAverageAmountAt,
    minutesAt,
    missingFeature,
    queryScale,
    toEncodedList,
    txCount24hAt,
    weekdayAt,
  )
where

import Data.Int (Int16)
import qualified Data.Vector.Storable as VS

type EncodedVector = VS.Vector Int16

newtype EncodedFeature = EncodedFeature
  { encodedFeatureValue :: Int16
  }
  deriving stock (Eq, Ord, Show)

data Dimension
  = AmountDim
  | InstallmentsDim
  | AmountVsAvgDim
  | HourDim
  | WeekdayDim
  | MinutesSinceLastDim
  | KmFromLastDim
  | KmFromHomeDim
  | TxCount24hDim
  | OnlineTerminalDim
  | CardPresentDim
  | UnknownMerchantDim
  | MccRiskDim
  | MerchantAvgAmountDim
  deriving stock (Bounded, Enum, Eq, Ord, Show)

newtype Amount = Amount Double
  deriving stock (Eq, Show)

newtype Installments = Installments Int
  deriving stock (Eq, Show)

newtype AmountVsAverage = AmountVsAverage Double
  deriving stock (Eq, Show)

newtype HourOfDay = HourOfDay Int
  deriving stock (Eq, Show)

newtype WeekdayMondayZero = WeekdayMondayZero Int
  deriving stock (Eq, Show)

newtype Minutes = Minutes Double
  deriving stock (Eq, Show)

newtype Kilometers = Kilometers Double
  deriving stock (Eq, Show)

newtype TxCount24h = TxCount24h Int
  deriving stock (Eq, Show)

newtype FeatureFlag = FeatureFlag Bool
  deriving stock (Eq, Show)

newtype MccRiskScore = MccRiskScore Double
  deriving stock (Eq, Show)

newtype MerchantAverageAmount = MerchantAverageAmount Double
  deriving stock (Eq, Show)

data FraudFeatures = FraudFeatures
  { fraudFeatureAmount :: !Amount,
    fraudFeatureInstallments :: !Installments,
    fraudFeatureAmountVsAverage :: !AmountVsAverage,
    fraudFeatureHour :: !HourOfDay,
    fraudFeatureWeekday :: !WeekdayMondayZero,
    fraudFeatureMinutesSinceLast :: !(Maybe Minutes),
    fraudFeatureKmFromLast :: !(Maybe Kilometers),
    fraudFeatureKmFromHome :: !Kilometers,
    fraudFeatureTxCount24h :: !TxCount24h,
    fraudFeatureOnlineTerminal :: !FeatureFlag,
    fraudFeatureCardPresent :: !FeatureFlag,
    fraudFeatureUnknownMerchant :: !FeatureFlag,
    fraudFeatureMccRisk :: !MccRiskScore,
    fraudFeatureMerchantAverageAmount :: !MerchantAverageAmount
  }
  deriving stock (Eq, Show)

data EncodedFeatures = EncodedFeatures
  { encodedAmount :: !EncodedFeature,
    encodedInstallments :: !EncodedFeature,
    encodedAmountVsAverage :: !EncodedFeature,
    encodedHour :: !EncodedFeature,
    encodedWeekday :: !EncodedFeature,
    encodedMinutesSinceLast :: !EncodedFeature,
    encodedKmFromLast :: !EncodedFeature,
    encodedKmFromHome :: !EncodedFeature,
    encodedTxCount24h :: !EncodedFeature,
    encodedOnlineTerminal :: !EncodedFeature,
    encodedCardPresent :: !EncodedFeature,
    encodedUnknownMerchant :: !EncodedFeature,
    encodedMccRisk :: !EncodedFeature,
    encodedMerchantAverageAmount :: !EncodedFeature
  }
  deriving stock (Eq, Show)

allDimensions :: [Dimension]
allDimensions = [minBound .. maxBound]

dimensionCount :: Int
dimensionCount = length allDimensions

dimensionIndex :: Dimension -> Int
dimensionIndex = fromEnum

queryScale :: Double
queryScale = 10000

encodeDimension :: Double -> Int16
encodeDimension x
  | x <= (-1) = -10000
  | otherwise = round (clamp01 x * queryScale)

encodeFraudFeatures :: FraudFeatures -> EncodedVector
encodeFraudFeatures features =
  VS.fromListN
    dimensionCount
    [ value (amountAt (fraudFeatureAmount features)),
      value (installmentsAt (fraudFeatureInstallments features)),
      value (amountVsAverageAt (fraudFeatureAmountVsAverage features)),
      value (hourAt (fraudFeatureHour features)),
      value (weekdayAt (fraudFeatureWeekday features)),
      value (maybe missingFeature minutesAt (fraudFeatureMinutesSinceLast features)),
      value (maybe missingFeature kilometersAt (fraudFeatureKmFromLast features)),
      value (kilometersAt (fraudFeatureKmFromHome features)),
      value (txCount24hAt (fraudFeatureTxCount24h features)),
      value (flagAt (fraudFeatureOnlineTerminal features)),
      value (flagAt (fraudFeatureCardPresent features)),
      value (flagAt (fraudFeatureUnknownMerchant features)),
      value (mccRiskAt (fraudFeatureMccRisk features)),
      value (merchantAverageAmountAt (fraudFeatureMerchantAverageAmount features))
    ]
  where
    value = encodedFeatureValue

encodedFeatures :: EncodedVector -> EncodedFeatures
encodedFeatures vector =
  EncodedFeatures
    { encodedAmount = encodedAt vector AmountDim,
      encodedInstallments = encodedAt vector InstallmentsDim,
      encodedAmountVsAverage = encodedAt vector AmountVsAvgDim,
      encodedHour = encodedAt vector HourDim,
      encodedWeekday = encodedAt vector WeekdayDim,
      encodedMinutesSinceLast = encodedAt vector MinutesSinceLastDim,
      encodedKmFromLast = encodedAt vector KmFromLastDim,
      encodedKmFromHome = encodedAt vector KmFromHomeDim,
      encodedTxCount24h = encodedAt vector TxCount24hDim,
      encodedOnlineTerminal = encodedAt vector OnlineTerminalDim,
      encodedCardPresent = encodedAt vector CardPresentDim,
      encodedUnknownMerchant = encodedAt vector UnknownMerchantDim,
      encodedMccRisk = encodedAt vector MccRiskDim,
      encodedMerchantAverageAmount = encodedAt vector MerchantAvgAmountDim
    }

encodedAt :: EncodedVector -> Dimension -> EncodedFeature
encodedAt query dim =
  EncodedFeature (query VS.! dimensionIndex dim)

toEncodedList :: EncodedVector -> [Int16]
toEncodedList = VS.toList

amountAt :: Amount -> EncodedFeature
amountAt (Amount amount) =
  encodeNormalized (amount / maxAmount)

installmentsAt :: Installments -> EncodedFeature
installmentsAt (Installments installments) =
  encodeNormalized (fromIntegral installments / maxInstallments)

amountVsAverageAt :: AmountVsAverage -> EncodedFeature
amountVsAverageAt (AmountVsAverage ratio) =
  encodeNormalized (ratio / maxAmountVsAverage)

hourAt :: HourOfDay -> EncodedFeature
hourAt (HourOfDay hour) =
  encodeNormalized (fromIntegral hour / maxHourOfDay)

weekdayAt :: WeekdayMondayZero -> EncodedFeature
weekdayAt (WeekdayMondayZero weekday) =
  encodeNormalized (fromIntegral weekday / maxWeekdayMondayZero)

minutesAt :: Minutes -> EncodedFeature
minutesAt (Minutes minutes) =
  encodeNormalized (minutes / maxMinutes)

kilometersAt :: Kilometers -> EncodedFeature
kilometersAt (Kilometers kilometers) =
  encodeNormalized (kilometers / maxKilometers)

txCount24hAt :: TxCount24h -> EncodedFeature
txCount24hAt (TxCount24h count) =
  encodeNormalized (fromIntegral count / maxTxCount24h)

flagAt :: FeatureFlag -> EncodedFeature
flagAt (FeatureFlag False) = encodeNormalized 0
flagAt (FeatureFlag True) = encodeNormalized 1

mccRiskAt :: MccRiskScore -> EncodedFeature
mccRiskAt (MccRiskScore risk) =
  encodeNormalized risk

merchantAverageAmountAt :: MerchantAverageAmount -> EncodedFeature
merchantAverageAmountAt (MerchantAverageAmount amount) =
  encodeNormalized (amount / maxMerchantAverageAmount)

missingFeature :: EncodedFeature
missingFeature =
  EncodedFeature (encodeDimension (-1))

encodeNormalized :: Double -> EncodedFeature
encodeNormalized =
  EncodedFeature . encodeDimension

clamp01 :: Double -> Double
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

maxAmount :: Double
maxAmount = 10000

maxInstallments :: Double
maxInstallments = 12

maxAmountVsAverage :: Double
maxAmountVsAverage = 10

maxHourOfDay :: Double
maxHourOfDay = 23

maxWeekdayMondayZero :: Double
maxWeekdayMondayZero = 6

maxMinutes :: Double
maxMinutes = 1440

maxKilometers :: Double
maxKilometers = 1000

maxTxCount24h :: Double
maxTxCount24h = 20

maxMerchantAverageAmount :: Double
maxMerchantAverageAmount = 10000
