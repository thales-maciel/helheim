{-# LANGUAGE OverloadedStrings #-}

module Helheim.Vectorize
  ( Dimension (..),
    EncodedVector,
    FraudFeatures (..),
    dimensionCount,
    encodeDimension,
    mccRisk,
    queryScale,
    toEncodedList,
    vectorize,
  )
where

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
  lastFeatures <- traverse lastTransactionFeatures (fraudRequestLastTransaction request)
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
    requestedAtText = transactionRequestedAt tx

    lastTransactionFeatures lastTx = do
      requestedAt <- parseIsoUtc requestedAtText
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

parseIsoUtc :: Text -> Either String UTCTime
parseIsoUtc value =
  maybe
    (Left ("invalid UTC timestamp: " <> T.unpack value))
    Right
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack value))

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
