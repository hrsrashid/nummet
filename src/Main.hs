module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           System.Environment (getArgs, getProgName)
import           Data.Bifunctor

import           Parser
import           Stringify
import           Library
import qualified SlaeGauss as SG
import qualified BivarInterpolNewton as BIN
import qualified SnlaePicard as SP


main :: IO ()
main = do
  args <- getArgs
  case args of
    [algo, input, output] -> do
      res <- launch algo input
      report output res 
    _ -> cliHelp


scalarMatrix = parseInput $ parseMatrix parseDecimal
vectorAndFuncMatrix = do
  v <- parseInput $ parseVector parseDecimal
  fm <- parseInput $ parseMatrix parseExpression
  return (v, fm)


launch :: MonadIO m => String -> String -> m (Either String String)
launch "slae" input = do
  inData <- parseFile scalarMatrix input
  return $ stringify <$> (first show . SG.compute =<< inData)

launch "snlae" input = do
  inData <- parseFile vectorAndFuncMatrix input
  return $ stringify <$> (first show . SP.compute =<< inData)
  
launch "interpolate" input = do
  inData <- parseFile scalarMatrix input
  return $ stringify . BIN.compute <$> inData

launch _ _ = return $ Left $ show NoAlgorithm


report _ (Left e) = do
  putStrLn e
  putStrLn "Failed."

report output (Right x) = do
  writeFile output x
    `catch` \(SomeException _) -> putStrLn x
  putStrLn "Done."


cliHelp :: IO ()
cliHelp = do
  putStr "Usage: "
  putStr =<< getProgName
  putStrLn $ " ALGO INPUT OUTPUT\n"
       ++ "ALGO\tone of:\n"
       ++     "\tslae\t\tSolve SLAE by Gaussian elimination\n"
       ++     "\tsnlae\t\tSolve SNLAE using Picard method\n"
       ++     "\tinterpolate\tNewton bivariate polynomial interpolation\n"
       ++ "\n"
       ++ "INPUT\tfilename of input data\n"
       ++ "OUTPUT\tfilename for ouput data\n"
