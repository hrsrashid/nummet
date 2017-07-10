module Library where

import           Data.List   (intercalate)
import qualified Data.Matrix as Mx
import           Data.Ord    (comparing)
import qualified Data.Vector as Vec

type Matrix = Mx.Matrix Double
type Vector = Vec.Vector Double
type Permutations = Vec.Vector Int

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
  show (ArgumentOutOfRange s) = "Argument of function is out of range: " ++ s
  show (Bunch errs) = intercalate "\n" $ map show errs


closeTo :: Double -> Double -> Bool
closeTo = (nearZero .) . (-)

nearZero :: Double -> Bool
nearZero x = abs x < epsilon

epsilon = 1e-5

lInftyNorm :: Vector -> Double
lInftyNorm = abs . Vec.maximumBy (comparing abs)

toLInftyNormUnit :: Vector -> Vector
toLInftyNormUnit v = Vec.map (/lInftyNorm v) v

mulMxByVec :: Matrix -> Vector -> Vector
mulMxByVec m v = Vec.fromList $ map (Vec.sum . Vec.zipWith (*) v) (Mx.toRows m)

vecAddConst :: Vector -> Double -> Vector
vecAddConst v c = Vec.map (+c) v

vvZipWith :: (Double -> Double -> Double) -> Vec.Vector Vector -> Vec.Vector Vector -> Vec.Vector Vector
vvZipWith = Vec.zipWith . Vec.zipWith

vvTranspose :: Vec.Vector Vector -> Vec.Vector Vector
vvTranspose = Vec.fromList . Mx.toColumns . (Mx.fromRows :: [Vector] -> Matrix) . Vec.toList
