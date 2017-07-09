module FunctionSpec where

import qualified Data.Matrix as Mx
import qualified Data.Vector as Vec
import           Function
import           Library
import           Test.Hspec


suite :: SpecWith ()
suite =
  describe "Function" $ do
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
