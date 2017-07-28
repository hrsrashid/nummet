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
    let compute = toCReal3 . fmap Mx.toLists . BodeShooting.compute

    it "solve { uʹ = u, vʹ = v} u(0) = 1 v(10) = 22026.46579" $
      compute (Vec.fromList [0, 10], Mx.fromLists
        [ [ simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          , simpleFunc "0" (Right . const 0)
          ]
        , [ simpleFunc "0" (Right . const 0)
          , simpleFunc "1" (Right . const 1)
          , simpleFunc "0" (Right . const 0)
          ]
        ], Mx.fromLists
        [ [ 1
          , 0
          , 1
          ]
        , [ 0
          , 1
          , 22026.46579
          ]
        ]) `shouldBe` Right (toCReal [[]])
