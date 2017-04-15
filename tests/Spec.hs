module Main where

import           BivarInterpolNewtonSpec as BINS
import           ParserSpec              as PS
import           SlaeGaussSpec           as SGS
import           SnlaePicardSpec         as SPS
import           StringifySpec           as SS
import           RectIntegralSpec        as RIS
import           EigenPowerSpec          as EPS
import           LibrarySpec             as LS
import           Test.Hspec


main :: IO ()
main = hspec $ do
  PS.suite
  SS.suite
  LS.suite
  SGS.suite
  BINS.suite
  SPS.suite
  RIS.suite
  EPS.suite
