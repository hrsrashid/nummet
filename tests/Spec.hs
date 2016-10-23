module Main where

import           BivarInterpolNewtonSpec as BINS
import           ParserSpec              as PS
import           SlaeGaussSpec           as SGS
import           StringifySpec           as SS
import           Test.Hspec


main :: IO ()
main = hspec $ do
  PS.suite
  SS.suite
  SGS.suite
  BINS.suite
