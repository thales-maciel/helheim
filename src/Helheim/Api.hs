{-# LANGUAGE OverloadedStrings #-}

module Helheim.Api
  ( runApi,
  )
where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Helheim.Engine
import Helheim.RequestParser (parseFraudRequest)
import Helheim.Types (FraudResponse (..))
import Helheim.Vectorize
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

runApi :: Int -> Engine -> IO ()
runApi port engine =
  runSettings settings (app engine)
  where
    settings =
      setPort port $
        setHost "*" $
          setServerName "helheim" $
            setTimeout 5 $
              defaultSettings

app :: Engine -> Application
app engine request respond =
  case (requestMethod request, pathInfo request) of
    ("GET", ["ready"]) ->
      respond (responseLBS status204 [] BL.empty)
    ("POST", ["fraud-score"]) -> do
      body <- strictRequestBody request
      respond (fraudScoreResponse engine body)
    _ ->
      respond (responseLBS status404 [jsonHeader] "{\"error\":\"not found\"}")

fraudScoreResponse :: Engine -> BL.ByteString -> Response
fraudScoreResponse engine body =
  case parseFraudRequest (BL.toStrict body) of
    Left err ->
      responseLBS status400 [jsonHeader] (encodeError err)
    Right fraudRequest ->
      case vectorize fraudRequest of
        Left err ->
          responseLBS status400 [jsonHeader] (encodeError err)
        Right query ->
          responseLBS status200 [jsonHeader] (encodeFraudResponse (classify engine fraudRequest query))

encodeFraudResponse :: FraudResponse -> BL.ByteString
encodeFraudResponse FraudResponse {fraudResponseScore = score}
  | score < 0.1 = "{\"approved\":true,\"fraud_score\":0.0}"
  | score < 0.3 = "{\"approved\":true,\"fraud_score\":0.2}"
  | score < 0.5 = "{\"approved\":true,\"fraud_score\":0.4}"
  | score < 0.7 = "{\"approved\":false,\"fraud_score\":0.6}"
  | score < 0.9 = "{\"approved\":false,\"fraud_score\":0.8}"
  | otherwise = "{\"approved\":false,\"fraud_score\":1.0}"

jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json")

encodeError :: String -> BL.ByteString
encodeError err =
  encode (object ["error" .= err])
