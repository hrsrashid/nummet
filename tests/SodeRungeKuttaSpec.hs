module SodeRungeKuttaSpec where

import qualified Data.Matrix    as Mx
import qualified Data.Vector    as Vec
import           Function
import           Library
import qualified SodeRungeKutta
import           Test.Hspec
import           TestLib


suite :: SpecWith ()
suite =
  describe "SodeRungeKutta" $ do
    let compute = fmap Mx.toLists . SodeRungeKutta.compute . Mx.fromLists

    xit "solve y' = {1, 2x}; y(0) = {0, 0}" $
      compute
        [ [simpleFunc "1"  (Right . const 1), simpleFunc "0" (Right . const 0)]
        , [simpleFunc "2x" (Right . (2*)),    simpleFunc "0" (Right . const 0)]
        ] `shouldBe` Right
        [ []
        , []
        ]
