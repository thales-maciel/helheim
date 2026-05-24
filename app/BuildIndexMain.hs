module Main (main) where

import Helheim.ReferenceBuilder
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  input <- requiredArg "--input" args
  output <- requiredArg "--output" args
  leafSize <- intArg "--leaf-size" args 64
  putStrLn ("leaf size: " <> show leafSize)
  stats <- buildIndexFromGzip input output leafSize
  putStrLn ("references: " <> show (buildStatsReferences stats))
  putStrLn ("frauds: " <> show (buildStatsFrauds stats))
  putStrLn ("legits: " <> show (buildStatsLegits stats))

requiredArg :: String -> [String] -> IO String
requiredArg name args =
  case dropWhile (/= name) args of
    (_ : value : _) -> pure value
    _ -> fail ("missing required argument " <> name)

intArg :: String -> [String] -> Int -> IO Int
intArg name args fallback =
  case dropWhile (/= name) args of
    (_ : value : _) ->
      case readMaybe value of
        Just n | n > 0 -> pure n
        _ -> fail ("invalid value for " <> name <> ": " <> value)
    _ -> pure fallback
