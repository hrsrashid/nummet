module Library where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec

type Matrix = Mx.Matrix CReal
type FMatrix = Mx.Matrix Function
type Vector = Vec.Vector CReal
type Permutations = Vec.Vector Int

newtype Function = Function { runFunction :: CReal -> CReal }

instance Show Function where
  show _ = "<function>"

instance Eq Function where
  Function f == Function g = and $ zipWith (==) (map f args) (map g args)
    where args = [1, 10 .. 100]

compose :: Function -> Function -> Function
compose f g = Function $ runFunction f . runFunction g

data ComputeError =
    NoAlgorithm
  | SingularMatrix
  deriving (Eq)

instance Show ComputeError where
  show NoAlgorithm = "No such algorithm found."
  show SingularMatrix = "Matrix is singular (degenerate, invertible)."

nearZero :: CReal -> Bool
nearZero x = abs x < 1e-16
