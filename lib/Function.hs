module Function where

import           Control.Monad        (liftM2, (<=<))
import           Data.Functor.Classes (liftEq2)
import qualified Data.Matrix          as Mx
import qualified Data.Vector          as Vec
import           Library

data Function = Function
  { showFunction :: String
  , runFunction  :: Vector -> Either ComputeError Double
  }

type FMatrix = Mx.Matrix Function

instance Show Function where
  show = take 100 . showFunction

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

compose2 :: Function -> Function -> Function -> Function
compose2 f g h = Function (show f ++ "(" ++ show g ++ "," ++ show h ++ ")") (\v ->
  runFunction f =<< (do
    gv <- runFunction g v
    hv <- runFunction h v
    return $ Vec.fromList [gv, hv]
  ))

simpleFunc :: String -> (Double -> Either ComputeError Double) -> Function
simpleFunc s f = Function s $ f . (Vec.! 0)

runMeshFunction :: Traversable t => Function -> t Vector -> Either ComputeError (t Double)
runMeshFunction = traverse . runFunction

runMeshFunctionSystem :: (Traversable v, Traversable t) => v Function -> t Vector -> Either ComputeError (v (t Double))
runMeshFunctionSystem fs xs = traverse (flip runMeshFunction xs) fs

runFunctionSystem :: Traversable t => t Function -> Vector -> Either ComputeError (t Double)
runFunctionSystem fs x = traverse (flip runFunction x) fs
