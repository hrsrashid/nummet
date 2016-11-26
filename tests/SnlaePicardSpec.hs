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
        [ [ Function (const 1)
          , Function (const 1)
          , Function (\v -> (v Vec.! 0) * (v Vec.! 1) - 5)
          ]
        , [ Function (const 2)
          , Function (const 3)
          , Function (\v -> (v Vec.! 0) * (v Vec.! 0) - 11)
          ]
        ] `shouldBe` [2, 1]
