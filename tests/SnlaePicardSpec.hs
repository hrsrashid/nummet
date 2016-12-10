module SnlaePicardSpec where

import Test.Hspec
import Library
import TestLib
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified SnlaePicard


suite :: SpecWith ()
suite =
  describe "SnlaePicard" $ do
    let compute = toCReal2 . fmap Vec.toList . SnlaePicard.compute . Mx.fromLists

    it "solve {x + y + x * y - 5 = 0, 2x + 3y + x * x - 11 = 0}" $
      compute
        [ [ Function "1" (const $ Right 1)
          , Function "1" (const $ Right 1)
          , Function "x0*x1-5" (\v -> Right $ (v Vec.! 0) * (v Vec.! 1) - 5)
          ]
        , [ Function "2" (const $ Right 2)
          , Function "3" (const $ Right 3)
          , Function "x0*x0-11" (\v -> Right $ (v Vec.! 0) * (v Vec.! 0) - 11)
          ]
        ] `shouldBe` toCReal2 (Right [2, 1])
