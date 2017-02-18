module RectIntegral where

import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Library


compute :: (Vector, Function) -> Double
compute (v, f) = sum $ map (go dx Nothing) [1 .. n]
  where
    go h int_2h_i i
      | acceptable = int_h_i
      | h < epsilon/2 = int_h_i
      | otherwise = go (h/2) (Just int_h_i) i
      where
        int_h_i = integr h i
        err = abs $ int_h_i - fromMaybe (integr (2*h) i) int_2h_i
        acceptable = err * (xn - x0) / h <= epsilon
  
    n = 500
    x0 = V.head v
    xn = V.last v
    dx = (xn - x0) / n
    xs h i =
      let
        xi = x0 + dx * (i - 1)
        xii = xi + dx
      in [xi, xi + h .. xii - h]

    ys xs =
      let
        values = partitionEithers 
          $ map (runFunction f . V.fromList . (:[])) xs
      in if not (null (fst values))
          then errorWithoutStackTrace "Failed: Function is discontinuous on the interval."
          else snd values

    integr h i = sum $ map (*h) $ ys $ xs h i

