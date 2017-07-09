module SodeRungeKutta where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import           Function
import           Library

-- f(x,y0,y1,...,yn)
-- x  y1  y2  y3  ...
-- -- --- --- --- ---
-- x1 y11 y12 y13 ...
-- x2 y21 y22 y23 ...
-- ... ... ... ...

compute :: FMatrix -> Either ComputeError Matrix
compute fm = undefined -- go x0 y0 0
  where
    -- go x_i y_i i
    --   | i < 10 = let y_i_1 = y x_i y_i in
    --     if V.all (==0) (V.zipWith (-) y_i y_i_1)
    --     then y_i_1
    --     else go x_i y_i_1 (i + 1)
    --   | otherwise = y_i

    sys_order :: Int
    sys_order = V.length f

    tau = 0.1
    f = M.takeColumn fm 0
    y_init = M.takeColumn fm 1

    x0 :: Vector
    x0 = V.fromList [1, 1 + tau .. 10]

    y0 :: Either ComputeError (V.Vector Vector)
    y0 = runMeshFunctionSystem y_init $ flip V.cons V.empty <$> x0

    y x0 y0 = do
      k1' <- k1 x0 y0
      k3' <- k3 x0 y0
      return $ (V.zipWith3 . V.zipWith3) (\k1_i_j k3_i_j y0_i_j -> tau*(k1_i_j + 3*k3_i_j)/4 + y0_i_j) k1' k3' y0

    k1 :: Vector -> V.Vector Vector -> Either ComputeError (V.Vector Vector)
    k1 x y = runMeshFunctionSystem f $ V.zipWith V.cons x y
    k2 x y = k1 x y >>= k1 (vecAddConst x (tau/3)) . vvZipWith (\y_i k1_i -> y_i + tau *   k1_i / 3) y
    k3 x y = k2 x y >>= k1 (vecAddConst x (2*tau/3)) . vvZipWith (\y_i k2_i -> y_i + tau * 2*k2_i / 3) y
