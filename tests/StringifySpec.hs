module StringifySpec where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec
import           Stringify
import           Test.Hspec


suite :: SpecWith ()
suite =
  describe "Stringification" $
    it "of vector" $
      stringify (Vec.fromList [1, 2, 3.5 :: CReal]) `shouldBe` "1.0\t2.0\t3.5"
