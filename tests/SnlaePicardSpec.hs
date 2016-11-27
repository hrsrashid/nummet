module SnlaePicardSpec where

import Test.Hspec
import Library
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified SnlaePicard


suite :: SpecWith ()
suite =
  describe "SnlaePicard" $ do
    let compute = fmap Vec.toList . SnlaePicard.compute . Mx.fromLists

    it "solve {x + y + x * y - 5 = 0, 2x + 3y + x * x - 11 = 0}" $
      compute
        [ [ Function "1" (const 1)
          , Function "1" (const 1)
          , Function "x0*x1-5" (\v -> (v Vec.! 0) * (v Vec.! 1) - 5)
          ]
        , [ Function "2" (const 2)
          , Function "3" (const 3)
          , Function "x0*x0-11" (\v -> (v Vec.! 0) * (v Vec.! 0) - 11)
          ]
        ] `shouldBe` Right [2, 1]
