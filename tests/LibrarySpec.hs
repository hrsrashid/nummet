module LibrarySpec where

import           Test.Hspec
import           Library
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx


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

    it "compares functions" $ do
      simpleFunc "sin" (Right . sin) `shouldBe` simpleFunc "sin" (Right . sin)
      simpleFunc "sin" (Right . sin) `shouldBe` simpleFunc "-cos(x+pi/2)" (Right . negate . cos . (+(pi/2)))
      simpleFunc "sin" (Right . sin) `shouldNotBe` simpleFunc "cos" (Right . cos)
      simpleFunc "sin" (Right . sin) `shouldNotBe` simpleFunc "x" (Right . id)

    it "adds functions" $
      (+) (simpleFunc "x" (Right . id)) (simpleFunc "x" (Right . id)) `shouldBe` simpleFunc "x+x" (Right . (*2))

    it "multiplies functions" $
      (*) (simpleFunc "x" (Right . id)) (simpleFunc "x" (Right . id)) `shouldBe` simpleFunc "x*x" (Right . (**2))

    it "divides functions" $
      (/) (simpleFunc "x" (Right . id)) (simpleFunc "x" (Right . id)) `shouldBe` simpleFunc "x/x" (Right . const 1)

    it "functions divison by zero returns Left value" $
      (/) (simpleFunc "x" (Right . id)) (simpleFunc "0" (Right . const 0)) `shouldBe` simpleFunc "const Left div by zero" (Left . const (ArgumentOutOfRange "Division by zero"))

    it "negates functions" $
      negate (simpleFunc "x" (Right . id)) `shouldBe` simpleFunc "-x" (Right . negate)

    it "module of functions" $
      abs (simpleFunc "-x" (Right . negate)) `shouldBe` simpleFunc "|x|" (Right . id)

    it "makes const function from integer" $
      fromInteger 4 `shouldBe` simpleFunc "4" (Right . const 4)
