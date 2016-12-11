module StringifySpec where

import qualified Data.Matrix       as Mx
import qualified Data.Vector       as Vec
import           Stringify
import           Test.Hspec


suite :: SpecWith ()
suite =
  describe "Stringification" $ do
    it "of vector" $
      stringify (Vec.fromList [1, 2, 3.5 :: Double]) `shouldBe` "1.00\t2.00\t3.50"

    it "of matrix" $
      stringify (Mx.matrix 3 [1..9] :: Mx.Matrix Double)
        `shouldBe` "1.00\t2.00\t3.00\n4.00\t5.00\t6.00\n7.00\t8.00\t9.00"
