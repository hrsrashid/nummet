module SnlaePicard where

import Data.Maybe
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx
import Data.Number.CReal
import Library
import qualified SlaeGauss as SG

compute :: FMatrix -> Either ComputeError Vector
compute fm = go 0 Nothing xs0
  where
    zeros = Vec.fromList $ replicate (Mx.rows fm) 0
    xs0 = either (const zeros) id $ SG.compute $ Mx.imap (mapLastCol (const 0)) (scalarMatrix zeros)

    go :: Int -> Maybe CReal -> Vector -> Either ComputeError Vector
    go iters d0 xs
      | iters >= 5 = Right xs
      | converges = xs'
      | diverges = Left (Divergence iters)
      | otherwise = go (iters + 1) (either (const Nothing) Just d) =<< xs'
      where
        xs' = SG.compute (Mx.imap (mapLastCol negate) (scalarMatrix xs))
        d = manhattanDistance xs <$> xs'

        diverges = case d of
                      Left _ -> False
                      Right d' -> d' - fromMaybe d' d0 > 1e+6

        converges = case d of
                      Left _ -> False
                      Right d' -> d' - fromMaybe 0 d0 < 1e-2

        manhattanDistance v1 v2 = Vec.sum $ Vec.map abs $ Vec.zipWith (-) v1 v2

    scalarMatrix :: Vector -> Matrix
    scalarMatrix xs = Mx.map (`runFunction` xs) fm

    mapLastCol :: (CReal -> CReal) -> (Int, Int) -> CReal -> CReal
    mapLastCol f (_, j) a
                | j+1 == Mx.cols fm = f a
                | otherwise = a
