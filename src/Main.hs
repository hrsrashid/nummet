module Main where

import           Control.Exception
import           System.Environment (getArgs)

import           Parser
import           Stringify
import qualified SlaeGauss as SG
import qualified BivarInterpolNewton as BIN


main :: IO ()
main = do
  args <- getArgs
  case args of
    [algo, input, output] -> do
      inputData <- parseFile (parseInput parseMatrix) input

      case inputData of
        Left e -> do
          print e
          putStrLn "Failed."

        Right parsedData -> do
          let result = case algo of
                        "slae" -> stringify $ SG.compute parsedData
                        "interpolate" -> stringify $ BIN.compute parsedData
                        _ -> "unknown algorithm"

          writeFile output result
            `catch` \(SomeException _) -> putStrLn result
          putStrLn "Done."

    _ -> putStrLn $ "Usage: ... ALGO INPUT OUTPUT\n"
      ++ "ALGO\tone of:\n"
      ++     "\tslae\t\tSolve SLAE by Gaussian elimination\n"
      ++     "\tinterpolate\tNewton bivariate polynomial interpolation\n"
      ++ "\n"
      ++ "INPUT\tfilename of input data\n"
      ++ "OUTPUT\tfilename for ouput data\n"
