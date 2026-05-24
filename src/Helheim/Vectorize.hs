{-# LANGUAGE OverloadedStrings #-}

module Helheim.Vectorize
  ( Dimension (..),
    EncodedVector,
    FraudFeatures (..),
    dimensionCount,
    encodeDimension,
    mccRisk,
    parseIsoUtc,
    queryScale,
    toEncodedList,
    vectorize,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Helheim.Features
import Helheim.Types

data LastTransactionFeatures = LastTransactionFeatures
  { lastMinutesSinceRequest :: !Minutes,
    lastKmFromRequest :: !Kilometers
  }

vectorize :: FraudRequest -> Either String EncodedVector
vectorize request = do
  requestedAt <- parseIsoUtc (transactionRequestedAt tx)
  lastFeatures <- traverse (lastTransactionFeatures requestedAt) (fraudRequestLastTransaction request)
  let merchantIsUnknown = merchantId merchant `notElem` customerKnownMerchants customer
      features =
        FraudFeatures
          { fraudFeatureAmount = Amount (transactionAmount tx),
            fraudFeatureInstallments = Installments (transactionInstallments tx),
            fraudFeatureAmountVsAverage = AmountVsAverage (amountVsCustomerAverage tx customer),
            fraudFeatureHour = hourOfDay requestedAt,
            fraudFeatureWeekday = dayOfWeekMondayZero requestedAt,
            fraudFeatureMinutesSinceLast = lastMinutesSinceRequest <$> lastFeatures,
            fraudFeatureKmFromLast = lastKmFromRequest <$> lastFeatures,
            fraudFeatureKmFromHome = Kilometers (terminalKmFromHome terminal),
            fraudFeatureTxCount24h = TxCount24h (customerTxCount24h customer),
            fraudFeatureOnlineTerminal = FeatureFlag (terminalIsOnline terminal),
            fraudFeatureCardPresent = FeatureFlag (terminalCardPresent terminal),
            fraudFeatureUnknownMerchant = FeatureFlag merchantIsUnknown,
            fraudFeatureMccRisk = MccRiskScore (mccRisk (merchantMcc merchant)),
            fraudFeatureMerchantAverageAmount = MerchantAverageAmount (merchantAvgAmount merchant)
          }
  pure (encodeFraudFeatures features)
  where
    tx = fraudRequestTransaction request
    customer = fraudRequestCustomer request
    merchant = fraudRequestMerchant request
    terminal = fraudRequestTerminal request

    lastTransactionFeatures requestedAt lastTx = do
      previousAt <- parseIsoUtc (lastTransactionTimestamp lastTx)
      let minutes = realToFrac (diffUTCTime requestedAt previousAt) / (60 :: Double)
      pure
        LastTransactionFeatures
          { lastMinutesSinceRequest = Minutes minutes,
            lastKmFromRequest = Kilometers (lastTransactionKmFromCurrent lastTx)
          }

amountVsCustomerAverage :: Transaction -> Customer -> Double
amountVsCustomerAverage tx customer =
  safeDiv (transactionAmount tx) (customerAvgAmount customer)

mccRisk :: Text -> Double
mccRisk "5411" = 0.15
mccRisk "5812" = 0.30
mccRisk "5912" = 0.20
mccRisk "5944" = 0.45
mccRisk "7801" = 0.80
mccRisk "7802" = 0.75
mccRisk "7995" = 0.85
mccRisk "4511" = 0.35
mccRisk "5311" = 0.25
mccRisk "5999" = 0.50
mccRisk _ = 0.50

-- | Parse a UTC timestamp. Tries a fixed-format fast path that constructs the
-- exact same 'UTCTime' 'parseTimeM' would; falls back to 'parseTimeM' for
-- anything not matching the strict @YYYY-MM-DDTHH:MM:SSZ@ shape (leap seconds,
-- invalid dates, odd input), preserving the original behaviour.
parseIsoUtc :: Text -> Either String UTCTime
parseIsoUtc value =
  case fastIsoUtc str of
    Just t -> Right t
    Nothing ->
      maybe
        (Left ("invalid UTC timestamp: " <> str))
        Right
        (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" str)
  where
    str = T.unpack value

-- | Fixed-format @YYYY-MM-DDTHH:MM:SSZ@ parser. Returns 'Nothing' (deferring to
-- 'parseTimeM') unless every position is exactly as expected, the field ranges
-- are valid, and the date is a real calendar date. By building the result with
-- 'fromGregorianValid' + 'secondsToDiffTime' it yields a 'UTCTime' identical to
-- 'parseTimeM' on the inputs it accepts. Excludes leap seconds (ss == 60).
fastIsoUtc :: String -> Maybe UTCTime
fastIsoUtc [c0, c1, c2, c3, '-', c5, c6, '-', c8, c9, 'T', c11, c12, ':', c14, c15, ':', c17, c18, 'Z']
  | all isDigit [c0, c1, c2, c3, c5, c6, c8, c9, c11, c12, c14, c15, c17, c18],
    hh <= 23,
    mi <= 59,
    ss <= 59 =
      case fromGregorianValid (fromIntegral yr) mo dy of
        Just day -> Just (UTCTime day (secondsToDiffTime (fromIntegral (hh * 3600 + mi * 60 + ss))))
        Nothing -> Nothing
  where
    d ch = fromEnum ch - fromEnum '0'
    yr = d c0 * 1000 + d c1 * 100 + d c2 * 10 + d c3 :: Int
    mo = d c5 * 10 + d c6
    dy = d c8 * 10 + d c9
    hh = d c11 * 10 + d c12
    mi = d c14 * 10 + d c15
    ss = d c17 * 10 + d c18
fastIsoUtc _ = Nothing

hourOfDay :: UTCTime -> HourOfDay
hourOfDay =
  HourOfDay . todHour . timeToTimeOfDay . utctDayTime

dayOfWeekMondayZero :: UTCTime -> WeekdayMondayZero
dayOfWeekMondayZero time =
  let (_, _, weekDay) = toWeekDate (utctDay time)
   in WeekdayMondayZero (weekDay - 1)

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 1 / 0
safeDiv a b = a / b
