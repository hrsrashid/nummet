module Stringify where

import Numeric
import Data.List (intercalate)
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic as VecG
import qualified Data.Matrix.Dense.Generic as Mx

class Stringifiable a where
  stringify :: a -> String
  stringify _ = "Failed to stringify unknown object"

instance Stringifiable Double where
  stringify = show

instance Stringifiable a => Stringifiable (Vec.Vector a) where
  stringify = intercalate "\t" . Vec.toList . fmap stringify

instance (Stringifiable (v a), VecG.Vector v a) => Stringifiable (Mx.Matrix v a) where
  stringify = intercalate "\n" . fmap stringify . Mx.toRows
