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
    let compute = toCReal3 . BodeShooting.compute . fmap Mx.fromLists

    it "solve yʹ = y; y(0) = 1; y(10) = 22026.46579" $
      compute (0.1, Vec.fromList [0, 10], Mx.toLists
        [ [ simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          ]
        ], Vec.fromList
        [ simpleFunc "1" (Right . const 1)
        , simpleFunc "1" (Right . const 1)
        ], Vec.fromList
        [ simpleFunc "1" (Right . const 0)
        , simpleFunc "22026.46579" (Right . const 22026.46579)
        ]) `shouldBe` Right (toCReal [[]])
