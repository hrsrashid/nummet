module StringifySpec where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec
import           Stringify
import           Test.Hspec


suite :: SpecWith ()
suite =
  describe "Stringification" $ do
    it "of vector" $
      stringify (Vec.fromList [1, 2, 3.5 :: CReal]) `shouldBe` "1.0\t2.0\t3.5"

    it "of matrix" $
      stringify (Mx.matrix 3 [1..9] :: Mx.Matrix CReal)
        `shouldBe` "1.0\t2.0\t3.0\n4.0\t5.0\t6.0\n7.0\t8.0\t9.0"
