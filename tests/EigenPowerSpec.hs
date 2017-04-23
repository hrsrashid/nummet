module EigenPowerSpec where

import           Data.Bifunctor (bimap)
import           Test.Hspec
import           Library
import           TestLib
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified EigenPower


suite :: SpecWith ()
suite =
  describe "EigenPower" $ do
    let compute = bimap CReal (fmap CReal) . fmap Vec.toList . EigenPower.compute . Mx.fromLists

    it "finds eigenpair for {{0.8, 0.3}, {0.2, 0.7}}" $
      compute [[0.8, 0.3], [0.2, 0.7]] `shouldBe` (CReal (1/2), CReal <$> [-1, 1])
