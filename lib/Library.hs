module Library where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec

type Matrix = Mx.Matrix CReal
type Vector = Vec.Vector CReal
type Permutations = Vec.Vector Int

data ComputeError =
    NoAlgorithm
  | SingularMatrix
  deriving (Eq)

instance Show ComputeError where
  show NoAlgorithm = "No such algorithm found."
  show SingularMatrix = "Matrix is singular (degenerate, invertible)."

nearZero :: CReal -> Bool
nearZero x = abs x < 1e-16
