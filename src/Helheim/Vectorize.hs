{-# LANGUAGE OverloadedStrings #-}

module Helheim.Vectorize
  ( EncodedVector,
    encodeDimension,
    mccRisk,
    queryScale,
    toEncodedList,
    vectorize,
  )
where

import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Data.Vector.Storable as VS
import Helheim.Types

type EncodedVector = VS.Vector Int16

queryScale :: Double
queryScale = 10000

encodeDimension :: Double -> Int16
encodeDimension x
  | x <= (-1) = -10000
  | otherwise = round (clamp01 x * queryScale)

toEncodedList :: EncodedVector -> [Int16]
toEncodedList = VS.toList

vectorize :: FraudRequest -> Either String EncodedVector
vectorize request = do
  requestedAt <- parseIsoUtc (transactionRequestedAt tx)
  lastPair <- traverse lastTransactionDimensions (fraudRequestLastTransaction request)
  let minutesSinceLast = maybe (-1) fst lastPair
      kmFromLast = maybe (-1) snd lastPair
      unknownMerchant =
        if merchantId merchant `elem` customerKnownMerchants customer
          then 0
          else 1
      dims =
        [ clamp01 (transactionAmount tx / maxAmount),
          clamp01 (fromIntegral (transactionInstallments tx) / maxInstallments),
          clamp01 ((safeDiv (transactionAmount tx) (customerAvgAmount customer)) / amountVsAvgRatio),
          hourOfDay requestedAt / 23,
          dayOfWeekMondayZero requestedAt / 6,
          minutesSinceLast,
          kmFromLast,
          clamp01 (terminalKmFromHome terminal / maxKm),
          clamp01 (fromIntegral (customerTxCount24h customer) / maxTxCount24h),
          if terminalIsOnline terminal then 1 else 0,
          if terminalCardPresent terminal then 1 else 0,
          unknownMerchant,
          mccRisk (merchantMcc merchant),
          clamp01 (merchantAvgAmount merchant / maxMerchantAvgAmount)
        ]
  pure (VS.fromList (fmap encodeDimension dims))
  where
    tx = fraudRequestTransaction request
    customer = fraudRequestCustomer request
    merchant = fraudRequestMerchant request
    terminal = fraudRequestTerminal request
    requestedAtText = transactionRequestedAt tx

    lastTransactionDimensions lastTx = do
      requestedAt <- parseIsoUtc requestedAtText
      previousAt <- parseIsoUtc (lastTransactionTimestamp lastTx)
      let minutes = realToFrac (diffUTCTime requestedAt previousAt) / (60 :: Double)
      pure
        ( clamp01 (minutes / maxMinutes),
          clamp01 (lastTransactionKmFromCurrent lastTx / maxKm)
        )

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

hourOfDay :: UTCTime -> Double
hourOfDay =
  fromIntegral . todHour . timeToTimeOfDay . utctDayTime

dayOfWeekMondayZero :: UTCTime -> Double
dayOfWeekMondayZero time =
  let (_, _, weekDay) = toWeekDate (utctDay time)
   in fromIntegral (weekDay - 1)

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 1 / 0
safeDiv a b = a / b

clamp01 :: Double -> Double
clamp01 x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

maxAmount :: Double
maxAmount = 10000

maxInstallments :: Double
maxInstallments = 12

amountVsAvgRatio :: Double
amountVsAvgRatio = 10

maxMinutes :: Double
maxMinutes = 1440

maxKm :: Double
maxKm = 1000

maxTxCount24h :: Double
maxTxCount24h = 20

maxMerchantAvgAmount :: Double
maxMerchantAvgAmount = 10000
