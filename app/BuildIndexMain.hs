module Main (main) where

import Helheim.ReferenceBuilder
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  input <- requiredArg "--input" args
  output <- requiredArg "--output" args
  stats <- buildIndexFromGzip input output
  putStrLn ("references: " <> show (buildStatsReferences stats))
  putStrLn ("frauds: " <> show (buildStatsFrauds stats))
  putStrLn ("legits: " <> show (buildStatsLegits stats))

requiredArg :: String -> [String] -> IO String
requiredArg name args =
  case dropWhile (/= name) args of
    (_ : value : _) -> pure value
    _ -> fail ("missing required argument " <> name)
