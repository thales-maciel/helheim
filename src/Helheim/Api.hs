{-# LANGUAGE OverloadedStrings #-}

module Helheim.Api
  ( runApi,
  )
where

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Helheim.Engine
import Helheim.RequestParser (parseFraudRequest)
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
          fraudResponseFor (classifyCount engine fraudRequest query)

-- Pre-baked responses, one per possible fraud count (0..5 neighbours). Built
-- once as CAFs; the hot path just selects one, avoiding per-request Response
-- construction, the Double score, and JSON encoding.
fraudResponseFor :: Int -> Response
fraudResponseFor 0 = response00
fraudResponseFor 1 = response02
fraudResponseFor 2 = response04
fraudResponseFor 3 = response06
fraudResponseFor 4 = response08
fraudResponseFor _ = response10

response00, response02, response04, response06, response08, response10 :: Response
response00 = bakedResponse "{\"approved\":true,\"fraud_score\":0.0}"
response02 = bakedResponse "{\"approved\":true,\"fraud_score\":0.2}"
response04 = bakedResponse "{\"approved\":true,\"fraud_score\":0.4}"
response06 = bakedResponse "{\"approved\":false,\"fraud_score\":0.6}"
response08 = bakedResponse "{\"approved\":false,\"fraud_score\":0.8}"
response10 = bakedResponse "{\"approved\":false,\"fraud_score\":1.0}"

bakedResponse :: BL.ByteString -> Response
bakedResponse = responseLBS status200 [jsonHeader]

jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json")

encodeError :: String -> BL.ByteString
encodeError err =
  encode (object ["error" .= err])
