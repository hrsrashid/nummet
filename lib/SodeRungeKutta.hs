module SodeRungeKutta where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import           Library

compute :: FMatrix -> [Function]
compute fm = mempty
  where
    tau = simpleFunc "0.1" (Right . const 0.1)
    f = M.takeColumn fm 0
    f0 = M.takeColumn fm 1
    k1 x y = V.zipWith3 compose2 f x y
    k2 x y = k1 (V.map (+   tau/3) x) (V.zipWith (\k1i yi -> yi + tau *   k1i / 3) (k1 x y) y)
    k3 x y = k1 (V.map (+ 2*tau/3) x) (V.zipWith (\k2i yi -> yi + tau * 2*k2i / 3) (k2 x y) y)
