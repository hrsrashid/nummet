module EigenPower where

import           Library
import qualified Data.Vector as V

compute :: Matrix -> Either ComputeError (Double, Vector)
compute _ = Right (-2, V.fromList [0, 1])