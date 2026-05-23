{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Helheim.Types
  ( Customer (..),
    FraudRequest (..),
    FraudResponse (..),
    LastTransaction (..),
    Merchant (..),
    Terminal (..),
    Transaction (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Transaction = Transaction
  { transactionAmount :: !Double,
    transactionInstallments :: !Int,
    transactionRequestedAt :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Transaction where
  parseJSON = withObject "transaction" $ \o ->
    Transaction
      <$> o .: "amount"
      <*> o .: "installments"
      <*> o .: "requested_at"

data Customer = Customer
  { customerAvgAmount :: !Double,
    customerTxCount24h :: !Int,
    customerKnownMerchants :: ![Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Customer where
  parseJSON = withObject "customer" $ \o ->
    Customer
      <$> o .: "avg_amount"
      <*> o .: "tx_count_24h"
      <*> o .: "known_merchants"

data Merchant = Merchant
  { merchantId :: !Text,
    merchantMcc :: !Text,
    merchantAvgAmount :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Merchant where
  parseJSON = withObject "merchant" $ \o ->
    Merchant
      <$> o .: "id"
      <*> o .: "mcc"
      <*> o .: "avg_amount"

data Terminal = Terminal
  { terminalIsOnline :: !Bool,
    terminalCardPresent :: !Bool,
    terminalKmFromHome :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Terminal where
  parseJSON = withObject "terminal" $ \o ->
    Terminal
      <$> o .: "is_online"
      <*> o .: "card_present"
      <*> o .: "km_from_home"

data LastTransaction = LastTransaction
  { lastTransactionTimestamp :: !Text,
    lastTransactionKmFromCurrent :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LastTransaction where
  parseJSON = withObject "last_transaction" $ \o ->
    LastTransaction
      <$> o .: "timestamp"
      <*> o .: "km_from_current"

data FraudRequest = FraudRequest
  { fraudRequestId :: !Text,
    fraudRequestTransaction :: !Transaction,
    fraudRequestCustomer :: !Customer,
    fraudRequestMerchant :: !Merchant,
    fraudRequestTerminal :: !Terminal,
    fraudRequestLastTransaction :: !(Maybe LastTransaction)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON FraudRequest where
  parseJSON = withObject "fraud_request" $ \o ->
    FraudRequest
      <$> o .: "id"
      <*> o .: "transaction"
      <*> o .: "customer"
      <*> o .: "merchant"
      <*> o .: "terminal"
      <*> o .: "last_transaction"

data FraudResponse = FraudResponse
  { fraudResponseApproved :: !Bool,
    fraudResponseScore :: !Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FraudResponse where
  toJSON response =
    object
      [ "approved" .= fraudResponseApproved response,
        "fraud_score" .= fraudResponseScore response
      ]
