module SlaeGaussSpec where

import Test.Hspec
import Library
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified SlaeGauss


suite :: SpecWith ()
suite = do
  describe "SlaeGauss" $ do
    let compute = fmap Vec.toList . SlaeGauss.compute . Mx.fromLists

    it "permute max element to (0, 0)" $
      SlaeGauss.permuteToMax 0 (Vec.fromList [0..2])
        [ Vec.fromList [1, 2, 3, 10]
        , Vec.fromList [4, 5, 6, 10]
        , Vec.fromList [7, 8, 9, 10]
        ]
      `shouldBe`
        ( [ Vec.fromList [9, 8, 7, 10]
          , Vec.fromList [6, 5, 4, 10]
          , Vec.fromList [3, 2, 1, 10]
          ]
        , Vec.fromList [2, 1, 0]
        )

    it "permute max element to (0, 1)" $
      SlaeGauss.permuteToMax 1 (Vec.fromList [0..2])
        [ Vec.fromList [1, 2, 3, 10]
        , Vec.fromList [9, 4, 8, 10]
        , Vec.fromList [7, 6, 5, 10]
        ]
      `shouldBe`
        ( [ Vec.fromList [9, 8, 4, 10]
          , Vec.fromList [1, 3, 2, 10]
          , Vec.fromList [7, 5, 6, 10]
          ]
        , Vec.fromList [0, 2, 1]
        )

    it "solve simple 2x2" $
      compute [[4, 5, 14], [99, 3, 105]] `shouldBe` Right [1, 2]

    it "solve 3x3 with fractional" $
      compute [[1, 1, 1, 3], [1, 2, 3.5, 6.5], [0.5, 0.1, 0.2, 0.8]]
        `shouldBe` Right [1, 1, 1]

    it "solve with fractional result" $
      compute [[1, 1, 1.6], [1, 2, 3.1]] `shouldBe` Right [0.1, 1.5]

    it "solve 3x3 x2" $
      compute [[10, 20, 0.5, 12], [9, 2, 1, 9], [10, 40, 1, 19]]
        `shouldBe` Right [0.5, 0.25, 4]

    it "solve with a11 = 0 (permutations required)" $
      compute
        [ [0, 2, 1, 11]
        , [1, 3, 2, 20]
        , [5, -2, 1, 4]
        ]
        `shouldBe` Right [1, 3, 5]

    it "fail triangulate singular matrix" $
      compute
        [ [1, 2, 3, 1]
        , [3, 5, 7, 1]
        , [1, 3, 5, 1]
        ]
        `shouldBe` Left SingularMatrix
