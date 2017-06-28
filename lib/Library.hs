module Library where

import Data.List (intercalate)
import Data.Ord (comparing)
import Data.Functor.Classes (liftEq2)
import Control.Monad (liftM2, (<=<))
import qualified Data.Matrix       as Mx
import qualified Data.Vector       as Vec

type Matrix = Mx.Matrix Double
type FMatrix = Mx.Matrix Function
type Vector = Vec.Vector Double
type Permutations = Vec.Vector Int

data Function = Function
  { showFunction :: String
  , runFunction :: Vector -> Either ComputeError Double
  }

instance Show Function where
  show = showFunction

instance Eq Function where
  Function _ f == Function _ g = and $ zipWith (liftEq2 (==) closeTo) (map f args) (map g args)
    where args = Vec.fromList <$> [[x, x + 10 .. x + 100] | x <- [1, 10 .. 100]]

instance Num Function where
  f + g = concat ["(", showFunction f, "+", showFunction g, ")"] `Function` (\x -> liftM2 (+) (runFunction f x) (runFunction g x))
  f * g = concat ["(", showFunction f, "*", showFunction g, ")"] `Function` (\x -> liftM2 (*) (runFunction f x) (runFunction g x))
  negate f = concat ["-", "(", showFunction f, ")"] `Function` (return . negate <=< runFunction f)
  abs f = compose (simpleFunc "abs" $ Right . abs) f
  signum f = compose (simpleFunc "signum" $ Right . signum) f
  fromInteger x = simpleFunc (show x) (Right . const (fromInteger x))

instance Fractional Function where
  f / g = concat ["(", showFunction f, "/", showFunction g, ")"] `Function` (\x -> do
            denominator <- runFunction g x
            numerator <- runFunction f x
            if nearZero denominator
            then Left $ ArgumentOutOfRange "Division by zero"
            else return $ numerator / denominator
          )
  fromRational x = simpleFunc (show x) (Right . const (fromRational x))

compose :: Function -> Function -> Function
compose f g = Function (show f ++ "(" ++ show g ++ ")") (\v -> runFunction f =<< (Vec.fromList . (: []) <$> runFunction g v))

simpleFunc :: String -> (Double -> Either ComputeError Double) -> Function
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
