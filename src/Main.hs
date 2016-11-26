module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           System.Environment (getArgs)
import           Data.Bifunctor

import           Parser
import           Stringify
import           Library
import qualified SlaeGauss as SG
import qualified BivarInterpolNewton as BIN


main :: IO ()
main = do
  args <- getArgs
  case args of
    [algo, input, output] -> (>>= launch algo) <$> parse input >>= report output
    _ -> putStrLn cliHelp


parse :: MonadIO m => String -> m (Either String Matrix)
parse = parseFile (parseInput parseMatrix)


launch :: String -> Matrix -> Either String String
launch algo input =
  case algo of
    "slae" -> bimap show stringify $ SG.compute input
    "slnae" -> Right "not impl"
    "interpolate" -> Right $ stringify $ BIN.compute input
    _ -> Left $ show NoAlgorithm


report :: String -> Either String String -> IO ()
report output result =
  case result of
    Left e -> do
      putStrLn e
      putStrLn "Failed."

    Right x -> do
      writeFile output x
        `catch` \(SomeException _) -> putStrLn x
      putStrLn "Done."


cliHelp :: String
cliHelp = "Usage: ... ALGO INPUT OUTPUT\n"
       ++ "ALGO\tone of:\n"
       ++     "\tslae\t\tSolve SLAE by Gaussian elimination\n"
       ++     "\tinterpolate\tNewton bivariate polynomial interpolation\n"
       ++ "\n"
       ++ "INPUT\tfilename of input data\n"
       ++ "OUTPUT\tfilename for ouput data\n"
