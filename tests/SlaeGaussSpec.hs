module SlaeGaussSpec where

import Test.Hspec
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified SlaeGauss


suite :: SpecWith ()
suite = do
  describe "SlaeGauss" $ do
    let compute = Vec.toList . SlaeGauss.compute . Mx.fromLists

    it "solve simple 2x2" $
      compute [[4, 5, 14], [99, 3, 105]] `shouldBe` [1, 2]

    it "solve 3x3 with fractional" $
      compute [[1, 1, 1, 3], [1, 2, 3.5, 6.5], [0.5, 0.1, 0.2, 0.8]]
        `shouldBe` [1, 1, 1]

    it "solve with fractional result" $
      compute [[1, 1, 1.6], [1, 2, 3.1]] `shouldBe` [0.1, 1.5]

    it "solve 3x3 x2" $
      compute [[1, 2, 3, 32.6], [10, 2, 1, 12.5], [2.5, 3, 1, 12.25]]
        `shouldBe` [0.1, 0.5, 10.5]

    it "solve 3x3 x3" $
      compute [[10, 20, 0.5, 12], [9, 2, 1, 9], [10, 40, 1, 19]]
        `shouldBe` [0.5, 0.25, 4]

    it "solve with a11 = 0 (permutations required)" $
      compute [[0, 5, 1, 10], [0.1, 3, 1, 9], [0.5, -5, 2, 10]]
        `shouldBe` [10, 1, 5]

    it "solve with permutations required" $
      compute [[2, 2, 4, 12], [3, 4, 2, 18], [1, 1, 1, 5.5]]
        `shouldBe` [3, 2, 0.5]
