module LibrarySpec where

import qualified Data.Matrix as Mx
import qualified Data.Vector as Vec
import           Library
import           Test.Hspec


suite :: SpecWith ()
suite =
  describe "Library" $ do
    it "compares 0.1 + 0.2 = 0.3" $
      nearZero (0.3 - (0.1 + 0.2)) `shouldBe` True

    it "finds correct L∞ norm of (0, -5, 3)" $
      lInftyNorm (Vec.fromList [0, -5, 3]) `shouldBe` 5

    it "finds correct L∞ norm of (-1, 3)" $
      lInftyNorm (Vec.fromList [-1, 3]) `shouldBe` 3

    it "find correct L∞ unit vector of (0, 5)" $
      toLInftyNormUnit (Vec.fromList [0, 5]) `shouldBe` Vec.fromList [0, 1]

    it "find correct L∞ unit vector of (3, -5)" $
      toLInftyNormUnit (Vec.fromList [3, -5]) `shouldBe` Vec.fromList [0.6, -1]

    it "multiplies {{3, 4}, {2, 8}} by (1, 2)" $
      Mx.fromLists [[3, 4], [2, 8]] `mulMxByVec` Vec.fromList [1, 2] `shouldBe` Vec.fromList [11, 18]
