module SodeRungeKutta where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import           Function
import           Library


compute :: (Double, FMatrix) -> Either ComputeError Matrix
compute (tau, fm) = M.fromRows
  <$> V.toList
  <$> V.zipWith V.cons xs
  <$> sequence (V.scanl y y0s xs)
  where
    sys_order :: Int
    sys_order = V.length f

    f = M.takeColumn fm 0
    y_init = M.takeColumn fm 1

    xs :: Vector
    xs = V.fromList [0, 0 + tau .. tau * 100]

    y0s :: Either ComputeError Vector
    y0s = runFunctionSystem y_init (V.fromList [0])

    y y0 x = do
      y0' <- y0
      k1' <- k1 x y0'
      k3' <- k3 x y0'
      return $ V.zipWith3 (\k1_i k3_i y0_i -> tau*(k1_i + 3*k3_i)/4 + y0_i) k1' k3' y0'

    k1 :: Double -> Vector -> Either ComputeError Vector
    k1 x y = runFunctionSystem f (V.cons x y)
    k2 x y = k1 x y >>= k1 (x +   tau/3) . V.zipWith (\y_i k1_i -> y_i + tau *   k1_i / 3) y
    k3 x y = k2 x y >>= k1 (x + 2*tau/3) . V.zipWith (\y_i k2_i -> y_i + tau * 2*k2_i / 3) y
