module Main (main) where

import Control.Applicative ((<|>))
import Helheim.Api
import Helheim.Engine
import Helheim.Index
import System.Environment (getArgs, lookupEnv)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  portEnv <- lookupEnv "PORT"
  indexEnv <- lookupEnv "HELHEIM_INDEX"
  engineEnv <- lookupEnv "HELHEIM_ENGINE"
  let port = optionInt "--port" args portEnv 8080
      indexPath = optionString "--index" args indexEnv "data/references.bin"
      mode = engineModeFromString (optionString "--engine" args engineEnv "exact")
  putStrLn ("loading reference index from " <> indexPath)
  index <- loadIndex indexPath
  putStrLn ("loaded " <> show (referenceCount index) <> " references")
  putStrLn ("engine mode: " <> show mode)
  runApi port Engine {engineMode = mode, engineIndex = index}

optionInt :: String -> [String] -> Maybe String -> Int -> Int
optionInt name args envValue fallback =
  maybe fallback id $
    (lookupArg name args >>= readMaybe)
      <|> (envValue >>= readMaybe)

optionString :: String -> [String] -> Maybe String -> String -> String
optionString name args envValue fallback =
  maybe fallback id (lookupArg name args <|> envValue)

lookupArg :: String -> [String] -> Maybe String
lookupArg name args =
  case dropWhile (/= name) args of
    (_ : value : _) -> Just value
    _ -> Nothing
