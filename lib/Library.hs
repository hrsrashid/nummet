module Library where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec

type Matrix = Mx.Matrix CReal
type Vector = Vec.Vector CReal
type Permutations = Vec.Vector Int
