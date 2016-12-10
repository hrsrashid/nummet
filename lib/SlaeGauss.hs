module SlaeGauss where

import           Data.Bifunctor
import qualified Data.Matrix       as Mx
import qualified Data.Vector       as Vec
import           Library


compute :: Matrix -> Either ComputeError Vector
compute = fmap backtrackPermuted . triangulate
  where
    backtrackPermuted (mx, permutations) =
      Vec.map (backtrack mx Vec.!) permutations


triangulate :: Matrix -> Either ComputeError (Matrix, Permutations)
triangulate mx = first Mx.fromRows <$> go 0 permutations rows
  where
    rows = Mx.toRows mx
    permutations = Vec.fromList [0..(length rows - 1)]

    go :: Int -> Permutations -> [Vector]
      -> Either ComputeError ([Vector], Permutations)
    go _ ps [] = Right ([], ps)
    go j ps m@(as:ass)
      | isLastCol = Right (m, ps)
      | isZeroPivot = Left SingularMatrix
      | otherwise = first (as':) <$> go (j + 1) ps' (map updRow ass')
      where
        isLastCol = j + 2 >= Vec.length as
        isZeroPivot = nearZero as'j
        as'j = as' Vec.! j
        (as':ass', ps') = permuteToMax j ps m

        updRow bs = Vec.zipWith op as' bs
          where
            k = bs Vec.! j / as'j
            op a b = b - a * k


permuteToMax :: Int -> Permutations -> [Vector] -> ([Vector], Permutations)
permuteToMax col ps m =
  ( Mx.toRows $ permuteRows 0 i $ permuteCols col j $ Mx.fromRows m
  , swapComponents col j ps
  )
  where
    (i, j, _) = foldr imax (0, 0, 0) $ zip [0..] m

    imax :: (Int, Vec.Vector Double) -> (Int, Int, Double) -> (Int, Int, Double)
    imax (i, v) oldMax@(_, _, max)
        | rowMax > max = (i, j + col, rowMax)
        | otherwise = oldMax
        where (j, rowMax) = Vec.ifoldl vimax (0, 0) (Vec.drop col $ Vec.init v)

    vimax :: (Int, Double) -> Int -> Double -> (Int, Double)
    vimax (i, max) k a
        | a > max = (k, a)
        | otherwise = (i, max)


permuteRows :: Int -> Int -> Matrix -> Matrix
permuteRows i k = Mx.fromColumns . map (swapComponents i k) . Mx.toColumns

permuteCols :: Int -> Int -> Matrix -> Matrix
permuteCols i k = Mx.fromRows . map (swapComponents i k) . Mx.toRows

swapComponents :: Int -> Int -> Vec.Vector a -> Vec.Vector a
swapComponents i k v = Vec.imap cmpSelector v
  where cmpSelector j el
                  | j == i = v Vec.! k
                  | j == k = v Vec.! i
                  | otherwise = el


backtrack :: Matrix -> Vector
backtrack = foldr eqSolver Vec.empty . Mx.toRows

eqSolver :: Vector -> Vector -> Vector
eqSolver as xs = Vec.cons ((Vec.last as' - Vec.sum (resolveKnown as' xs)) / Vec.head as') xs
  where as' = Vec.dropWhile nearZero as

resolveKnown :: Vector -> Vector -> Vector
resolveKnown as xs = Vec.zipWith (*) xs (Vec.tail $ Vec.init as)
