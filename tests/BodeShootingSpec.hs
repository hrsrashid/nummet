module BodeShootingSpec where

import qualified BodeShooting
import qualified Data.Matrix  as Mx
import qualified Data.Vector  as Vec
import           Function
import           Library
import           Test.Hspec
import           TestLib


suite :: SpecWith ()
suite =
  describe "BodeShooting" $ do
    let compute = toCReal3 . fmap Mx.toLists . BodeShooting.compute . fmap Mx.fromLists

    it "solve yʺ + yʹ + y = 0; y(0) = 0; y(10) = 0" $
      compute (0.1, Vec.fromList [0, 10]
        [ [ simpleFunc "1" (Right . const 1)
          , simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          , simpleFunc "0" (Right . const 0)
          , simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          , simpleFunc "0" (Right . const 0)
          , simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          ]
        ]) `shouldBe` Right (toCReal [[]])
