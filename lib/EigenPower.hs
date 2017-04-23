module EigenPower where

import           Library
import           Data.Bifunctor (first)
import qualified Data.Vector as V
import qualified Data.Matrix as M

compute :: Matrix -> (Double, Vector)
compute m =
    first (+dominant_l)
  $ computeDominantPair
  $ M.zipWith (-) m (M.diag (take (M.rows m) (repeat dominant_l)))
  where
    (dominant_l, _) = computeDominantPair m


computeDominantPair :: Matrix -> (Double, Vector)
computeDominantPair m = go (V.replicate (M.rows m) 0.0) (V.fromList [1 .. fromIntegral $ M.rows m])
  where
    go l y
      | converges l next_l = (avg next_l, next_x)
      | otherwise = go next_l next_y
      where
        x = toLInftyNormUnit y
        next_y = m `mulMxByVec` x
        next_x = toLInftyNormUnit next_y
        next_l = V.imap (\i (a,b) -> if nearZero b then l V.! i else a/b) $ V.zip next_y x

        converges l1 l2 = V.all nearZero $ V.zipWith (-) l1 l2
        avg v =
          let v' = V.filter (not . nearZero) v
          in if V.null v' then 0.0 else V.sum v' / fromIntegral (V.length v')
