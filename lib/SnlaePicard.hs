module SnlaePicard where

import Data.Maybe
import Data.Either (partitionEithers)
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import qualified Data.Matrix.Generic as DMG (fromList)
import Library
import qualified SlaeGauss as SG

compute :: (Vector, FMatrix) -> Either ComputeError Vector
compute (xs0, fm) = go 0 Nothing xs0
  where
    go :: Int -> Maybe Double -> Vector -> Either ComputeError Vector
    go iters d0 xs
      | converged || iters >= 1000 = Right xs
      | diverges = Left (Divergence iters)
      | otherwise = go (iters + 1) (either (const Nothing) Just d) =<< xs'
      where
        xs' = SG.compute =<< (Mx.imap (mapLastCol negate) <$> scalarMatrix xs)
        d = manhattanDistance xs <$> xs'

        diverges = case d of
                      Left _ -> False
                      Right d' -> d' - fromMaybe d' d0 > 1e+10

        converged = case d of
                      Left _ -> False
                      Right d' -> d' == 0

        manhattanDistance v1 v2 = Vec.sum $ Vec.map abs $ Vec.zipWith (-) v1 v2

    scalarMatrix :: Vector -> Either ComputeError Matrix
    scalarMatrix xs = if null errs
      then Right (DMG.fromList (Mx.dim fm) ys)
      else Left (Bunch errs)
      where
        (errs, ys) = partitionEithers $ Mx.toList $ Mx.map (`runFunction` xs) fm
         

    mapLastCol :: (Double -> Double) -> (Int, Int) -> Double -> Double
    mapLastCol f (_, j) a
                | j+1 == Mx.cols fm = f a
                | otherwise = a
