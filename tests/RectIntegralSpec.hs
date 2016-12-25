module RectIntegralSpec where

import           Data.Bifunctor (first)
import Control.Exception (evaluate)
import qualified Data.Vector as Vec
import qualified RectIntegral
import           Test.Hspec
import           TestLib
import qualified Library as Lib


suite :: SpecWith ()
suite = describe "Definite integral. Rectangular rule." $ do
  let compute = CReal . RectIntegral.compute . first Vec.fromList

  it "2*x from 1 to 2" $
    compute ([1, 2], Lib.simpleFunc "2*x" (Right . (2*))) `shouldBe` CReal 3

  it "1/x from exp(-1) to exp(1)" $
    compute ([exp(-1), exp 1], Lib.simpleFunc "1/x" (Right . (1/))) `shouldBe` CReal 2
