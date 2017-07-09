module SnlaePicardSpec where

import qualified Data.Matrix as Mx
import qualified Data.Vector as Vec
import           Function
import           Library
import qualified SnlaePicard
import           Test.Hspec
import           TestLib


suite :: SpecWith ()
suite =
  describe "SnlaePicard" $ do
    let compute x0 = toCReal2 . fmap Vec.toList . curry SnlaePicard.compute x0 . Mx.fromLists

    it "solve {x + y + sin(x) * y = 0, 2x + 3y + x * x + y * y = 0}" $
      compute (Vec.fromList [1, 1])
        [ [ Function "1" (const $ Right 1)
          , Function "1" (const $ Right 1)
          , Function "sin(x0)*x1" (\v -> Right $ sin (v Vec.! 0) * (v Vec.! 1))
          ]
        , [ Function "2" (const $ Right 2)
          , Function "3" (const $ Right 3)
          , Function "x0*x0+x1*x1" (\v -> Right $ (v Vec.! 0) * (v Vec.! 0) + (v Vec.! 1) * (v Vec.! 1))
          ]
        ] `shouldBe` toCReal2 (Right [0, 0])
