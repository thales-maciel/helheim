{-# LANGUAGE OverloadedStrings #-}

module Helheim.Api
  ( runApi,
  )
where

import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Helheim.Engine
import Helheim.Types
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
  case eitherDecode body :: Either String FraudRequest of
    Left err ->
      responseLBS status400 [jsonHeader] (encodeError err)
    Right fraudRequest ->
      case vectorize fraudRequest of
        Left err ->
          responseLBS status400 [jsonHeader] (encodeError err)
        Right query ->
          responseLBS status200 [jsonHeader] (encode (classify engine fraudRequest query))

jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json")

encodeError :: String -> BL.ByteString
encodeError err =
  encode (object ["error" .= err])
