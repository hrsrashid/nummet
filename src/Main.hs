module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           System.Environment     (getArgs, getProgName)

import qualified BivarInterpolNewton    as BIN
import qualified EigenPower             as EP
import           Library
import           Parser
import qualified RectIntegral           as RI
import qualified SlaeGauss              as SG
import qualified SnlaePicard            as SP
import qualified SodeRungeKutta         as SRK
import           Stringify


main :: IO ()
main = do
  args <- getArgs
  case args of
    [algo, input, output] -> do
      res <- launch algo input
      report output res
    _ -> cliHelp


scalarMatrix = parseInput $ parseMatrix parseDecimal
funcMatrix = parseInput $ parseMatrix parseExpression
vectorAndFuncMatrix = do
  v <- parseInput $ parseVector parseDecimal
  fm <- funcMatrix
  return (v, fm)
vectorAndFunc = do
  f <- parseInput parseExpression
  v <- parseInput $ parseVector parseDecimal
  return (v, f)
numberAndFuncMatrix = do
  x <- parseInput parseDecimal
  fm <- funcMatrix
  return (x, fm)


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

launch "int" input = do
  inData <- parseFile vectorAndFunc input
  return $ stringify . RI.compute <$> inData

launch "eigen" input = do
  inData <- parseFile scalarMatrix input
  return $ stringify <$> EP.compute <$> inData

launch "sode" input = do
  inData <- parseFile numberAndFuncMatrix input
  return $ stringify <$> (first show . SRK.compute =<< inData)

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
       ++     "\tint\t\tDefinite integral. Rectangular rule.\n"
       ++     "\teigen\t\tEigenpair. Power method.\n"
       ++     "\tsode\t\tSystems of ODE. Runge-Kutta method. 3rd order (2nd var).\n"
       ++ "\n"
       ++ "INPUT\tfilename of input data\n"
       ++ "OUTPUT\tfilename for ouput data\n"
