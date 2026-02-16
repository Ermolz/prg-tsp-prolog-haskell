-- | TSP CLI: --algo bruteforce --file <path>
-- Output: cost=<int> tour=[1,...,1] (no spaces in tour list)
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import TSP.BruteForce (solveBruteForce)
import TSP.IO (readInstanceFile, validateMatrix)

usage :: String
usage = "Usage: tsp-exe --algo bruteforce --file <path>"

-- | Parse args: [--algo, bruteforce, --file, path] or [--file, path, --algo, bruteforce]
parseArgs :: [String] -> Either String (String, FilePath)
parseArgs args = do
  algo <- findArg args "--algo"
  file <- findArg args "--file"
  if algo == "bruteforce"
    then Right (algo, file)
    else Left $ "Unknown algo: " ++ algo ++ ". Use bruteforce."

findArg :: [String] -> String -> Either String String
findArg [] _ = Left "Missing argument"
findArg [x] _ = Left "Missing argument"
findArg (opt : val : rest) target
  | opt == target = Right val
  | otherwise = findArg (val : rest) target

-- | Format output: cost=17 tour=[1,2,4,5,3,1] (no spaces in list)
formatResult :: Int -> [Int] -> String
formatResult cost tour =
  "cost=" ++ show cost ++ " tour=" ++ formatTour tour

formatTour :: [Int] -> String
formatTour xs = "[" ++ showListNoSpaces xs ++ "]"

showListNoSpaces :: [Int] -> String
showListNoSpaces [] = ""
showListNoSpaces [x] = show x
showListNoSpaces (x : xs) = show x ++ "," ++ showListNoSpaces xs

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      putStrLn usage
      putStrLn err
      exitFailure
    Right ("bruteforce", path) -> do
      result <- readInstanceFile path
      case result of
        Left err -> do
          putStrLn $ "Error reading file: " ++ err
          exitFailure
        Right (n, matrix) ->
          case validateMatrix n matrix of
            Left err -> do
              putStrLn $ "Error validating: " ++ err
              exitFailure
            Right () -> do
              let (cost, tour) = solveBruteForce matrix
              putStrLn $ formatResult cost tour
