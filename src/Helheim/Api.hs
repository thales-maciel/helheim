{-# LANGUAGE OverloadedStrings #-}

module Helheim.Api
  ( runApi,
  )
where

import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Helheim.Index
import Helheim.Types
import Helheim.Vectorize
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

runApi :: Int -> ReferenceIndex -> IO ()
runApi port index =
  runSettings settings (app index)
  where
    settings =
      setPort port $
        setHost "*" $
          setServerName "helheim" $
            defaultSettings

app :: ReferenceIndex -> Application
app index request respond =
  case (requestMethod request, pathInfo request) of
    ("GET", ["ready"]) ->
      respond (responseLBS status204 [] BL.empty)
    ("POST", ["fraud-score"]) -> do
      body <- strictRequestBody request
      respond (fraudScoreResponse index body)
    _ ->
      respond (responseLBS status404 [jsonHeader] "{\"error\":\"not found\"}")

fraudScoreResponse :: ReferenceIndex -> BL.ByteString -> Response
fraudScoreResponse index body =
  case eitherDecode body :: Either String FraudRequest of
    Left err ->
      responseLBS status400 [jsonHeader] (encodeError err)
    Right fraudRequest ->
      case vectorize fraudRequest of
        Left err ->
          responseLBS status400 [jsonHeader] (encodeError err)
        Right query ->
          responseLBS status200 [jsonHeader] (encode (searchIndex index query))

jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json")

encodeError :: String -> BL.ByteString
encodeError err =
  encode (object ["error" .= err])
