{-# LANGUAGE DerivingStrategies #-}

module Helheim.Engine
  ( Engine (..),
    EngineMode (..),
    classify,
    engineModeFromString,
  )
where

import Data.Int (Int16)
import Helheim.Index
import Helheim.Types
import Helheim.Vectorize
import qualified Data.Vector.Storable as VS

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
      case shortcut request query of
        Just response -> response
        Nothing -> searchIndex (engineIndex engine) query

shortcut :: FraudRequest -> EncodedVector -> Maybe FraudResponse
shortcut request query
  | clearLegit request query = Just (FraudResponse True 0.0)
  | clearFraud request query = Just (FraudResponse False 1.0)
  | otherwise = Nothing

clearLegit :: FraudRequest -> EncodedVector -> Bool
clearLegit request query =
  dim 0 <= 600
    && dim 1 <= 3334
    && dim 2 <= 600
    && dim 7 <= 600
    && dim 8 <= 2500
    && dim 11 == 0
    && dim 12 <= 3000
    && businessHour
    && lastLooksLegit
  where
    dim = vectorDim query
    businessHour = dim 3 >= 3000 && dim 3 <= 8700
    lastLooksLegit =
      case fraudRequestLastTransaction request of
        Nothing -> True
        Just _ -> dim 6 <= 400

clearFraud :: FraudRequest -> EncodedVector -> Bool
clearFraud _ query =
  dim 0 >= 3000
    && dim 1 >= 5000
    && dim 2 >= 8000
    && dim 3 <= 2609
    && dim 7 >= 3000
    && dim 8 >= 4000
    && dim 11 == 10000
    && dim 12 >= 7500
    && (dim 5 == -10000 || dim 6 >= 2000)
  where
    dim = vectorDim query

vectorDim :: EncodedVector -> Int -> Int16
vectorDim query dim =
  query VS.! dim
