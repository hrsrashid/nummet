module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           System.Environment     (getArgs, getProgName)

import qualified BivarInterpolNewton    as BIN
import qualified BodeShooting           as BS
import qualified EigenPower             as EP
import qualified HeatExplicit           as HE
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
vector = parseInput $ parseVector parseDecimal
function = parseInput parseExpression
number = parseInput parseDecimal
vectorAndFuncMatrix = do
  v <- vector
  fm <- funcMatrix
  return (v, fm)
vectorAndFunc = do
  f <- function
  v <- vector
  return (v, f)
numberAndFuncMatrix = do
  x <- number
  fm <- funcMatrix
  return (x, fm)
vectorAndFuncMatrixAndMatrix = do
  domain <- vector
  fm <- funcMatrix
  bounds <- scalarMatrix
  return (domain, fm, bounds)
numberAnd4Funcs = do
  x <- number
  f1 <- function
  f2 <- function
  f3 <- function
  f4 <- function
  return (x, f1, f2, f3, f4)



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

launch "bode" input = do
  inData <- parseFile vectorAndFuncMatrixAndMatrix input
  return $ stringify <$> (first show . BS.compute =<< inData)

launch "heat" input = do
  inData <- parseFile numberAnd4Funcs input
  return $ stringify <$> (first show . HE.compute =<< inData)

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
       ++     "\tbode\t\tSystem of two 1st order linear ODE. Shooting method.\n"
       ++     "\theat\t\tHeat equation. Explicit method.\n"
       ++ "\n"
       ++ "INPUT\tfilename of input data\n"
       ++ "OUTPUT\tfilename for ouput data\n"
