module RectIntegral where

import Data.Either (partitionEithers)
import qualified Data.Vector as V
import Library


compute :: (Vector, Function) -> Double
compute (v, f) = sum $ map (*h) $ snd $ partitionEithers $ map (runFunction f) $ map V.fromList $ map (:[]) [V.head v, V.head v + h .. V.last v]
  where 
    h = 1e-3

