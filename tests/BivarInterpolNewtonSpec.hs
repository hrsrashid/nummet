module BivarInterpolNewtonSpec where

import qualified BivarInterpolNewton
import qualified Data.Matrix         as Mx
import           Test.Hspec


suite :: SpecWith ()
suite = describe "Newton bivariate polynomial interpolation" $ do
  let compute = Mx.toLists . BivarInterpolNewton.compute . Mx.fromLists

  it "approx correctly" $
    compute [[1, 2, 3], [4, 5, 6]] `shouldBe` [[]]
