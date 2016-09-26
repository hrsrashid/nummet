module Stringify where

import Data.Number.CReal
import Data.List (intercalate)
import qualified Data.Vector as Vec
import qualified Data.Matrix.Dense.Generic as Mx

class Stringifiable a where
  stringify :: a -> String
  stringify _ = "Failed to stringify unknown object"

instance Stringifiable CReal where
  stringify = showCReal 4

instance Stringifiable a => Stringifiable (Vec.Vector a) where
  stringify = intercalate "\t" . Vec.toList . fmap stringify

instance Stringifiable (v a) => Stringifiable (Mx.Matrix v a) where
  stringify _ = "Matrix stringification not implemented yet"
