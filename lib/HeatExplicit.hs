module HeatExplicit where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import           Function
import           Library

compute :: (Double, Function, Function, Function, Function) -> Either ComputeError Matrix
compute (sup_t, f, u_x_0, u_0_t, u_1_t) = M.fromVector (h_count + 1, tau_count + 1)
  <$> V.fromList
  <$> sequence [y i n | i <- [0..h_count], n <- [0..tau_count]]
  where
    h_count :: Int
    h_count = truncate $ sqrt $ fromIntegral tau_count / 2.0 / sup_t -- τ ≤ 0.5h²
    tau_count = 100
    tau = sup_t / fromIntegral tau_count
    h = 1.0 / fromIntegral h_count

    arg :: Int -> Int -> Vector
    arg i n = V.fromList [x i, t n]

    x i = h   * fromIntegral i
    t n = tau * fromIntegral n

    y :: Int -> Int -> Either ComputeError Double
    y i n
        | x i `closeTo` 0 = runFunction u_0_t (arg i n)
        | x i `closeTo` 1 = runFunction u_1_t (arg i n)
        | t n `closeTo` 0 = runFunction u_x_0 (arg i n)
        | otherwise = do
          y_i_1n <- y i (n-1)
          f_i_1n <- runFunction f $ arg i (n-1)
          y_i1_1n <- y (i+1) (n-1)
          y_1i_1n <- y (i-1) (n-1)
          return $ y_i_1n + tau*(f_i_1n + (y_i1_1n - 2 * y_i_1n + y_1i_1n)/h**2)
