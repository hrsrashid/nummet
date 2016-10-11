module Main where

import Test.Hspec
import ParserSpec as PS
import StringifySpec as SS
import SlaeGaussSpec as SGS
import BivarInterpolNewtonSpec as BINS


main :: IO ()
main = hspec $ do
  PS.suite
  SS.suite
  SGS.suite
  BINS.suite
