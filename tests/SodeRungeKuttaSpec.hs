module SodeRungeKuttaSpec where

import Test.Hspec
import Library
import TestLib
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified SodeRungeKutta


suite :: SpecWith ()
suite =
  describe "SodeRungeKutta" $ do
    let compute = SodeRungeKutta.compute . Mx.fromLists

    xit "solve y' = {1, 2x}; y(0) = {0, 0}" $
      compute
        [ [simpleFunc "1"  (Right . const 1), simpleFunc "0" (Right . const 0)]
        , [simpleFunc "2x" (Right . (2*)),    simpleFunc "0" (Right . const 0)]
        ] `shouldBe`
        [ simpleFunc "x" (Right . id)
        , simpleFunc "x*x" (Right . (**2))
        ]
