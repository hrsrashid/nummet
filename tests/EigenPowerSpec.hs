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
    let compute = fmap (bimap CReal (fmap CReal)) . fmap (fmap Vec.toList) . EigenPower.compute . Mx.fromLists

    it "finds eigenpair for {{1, 0}, {0, -2}}" $
      compute [[1, 0], [0, -2]] `shouldBe` Right (CReal (-2), CReal <$> [0, 1])
