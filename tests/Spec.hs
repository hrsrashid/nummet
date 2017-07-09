module Main where

import           BivarInterpolNewtonSpec as BINS
import           EigenPowerSpec          as EPS
import           FunctionSpec            as FS
import           LibrarySpec             as LS
import           ParserSpec              as PS
import           RectIntegralSpec        as RIS
import           SlaeGaussSpec           as SGS
import           SnlaePicardSpec         as SPS
import           SodeRungeKuttaSpec      as SRKS
import           StringifySpec           as SS
import           Test.Hspec


main :: IO ()
main = hspec $ do
  PS.suite
  SS.suite
  LS.suite
  FS.suite
  SGS.suite
  BINS.suite
  SPS.suite
  RIS.suite
  EPS.suite
  SRKS.suite
