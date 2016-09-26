module Main where

import System.Environment (getArgs)
import Control.Exception
import Text.Trifecta

import Parser
import Stringify
import SlaeGauss

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      inputData <- parseFromFileEx (parseInput parseMatrix) input

      case inputData of
        Failure e -> do
          putStrLn $ show e
          putStrLn "Failed."

        Success parsedData -> do
          let result = stringify $ compute parsedData
          writeFile output result
            `catch` \(SomeException _) -> putStrLn result
          putStrLn "Done."

    _ -> putStrLn $ "Usage: ... INPUT OUTPUT\n"
      ++ "INPUT - filename of input data\n"
      ++ "OUTPUT - filename for ouput data\n"
