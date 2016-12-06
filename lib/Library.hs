module Library where

import Data.List (intercalate)
import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec

type Matrix = Mx.Matrix CReal
type FMatrix = Mx.Matrix Function
type Vector = Vec.Vector CReal
type Permutations = Vec.Vector Int

data Function = Function
  { showFunction :: String
  , runFunction :: Vector -> Either ComputeError CReal
  }

instance Show Function where
  show = showFunction

instance Eq Function where
  Function _ f == Function _ g =
      either (const False) id 
    $ foldr (\a acc -> (&&) <$> a <*> acc) (Right True)
    $ zipWith (\x y -> (==) <$> x <*> y) (map f args) (map g args)
    where args = Vec.fromList <$> [[1, 10 .. 100] | x <- [1..10]]

compose :: Function -> Function -> Function
compose f g = Function (show f ++ "(" ++ show g ++ ")") (\v -> runFunction f =<< (Vec.fromList . (: []) <$> runFunction g v))

simpleFunc :: String -> (CReal -> Either ComputeError CReal) -> Function
simpleFunc s f = Function s $ f . (Vec.! 0)

data ComputeError =
    NoAlgorithm
  | SingularMatrix
  | Divergence Int
  | ArgumentOutOfRange String
  | Bunch [ComputeError]
  deriving (Eq)

instance Show ComputeError where
  show NoAlgorithm = "No such algorithm found."
  show SingularMatrix = "Matrix is singular (degenerate, invertible)."
  show (Divergence i) = "Unable to compute, algorithm diverges (" ++ show i ++ " iterations)"
  show (ArgumentOutOfRange s) = "Argument to function out of range: " ++ s
  show (Bunch errs) = intercalate "\n" $ map show errs

nearZero :: CReal -> Bool
nearZero x = abs x < 1e-16
