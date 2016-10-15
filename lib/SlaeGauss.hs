module SlaeGauss where

import Data.Number.CReal
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx

type Matrix = Mx.Matrix CReal
type Vector = Vec.Vector CReal
type Permutations = Vec.Vector Int


compute :: Matrix -> Vector
compute mx = Vec.map (backtrack mx' Vec.!) permutations
  where
    (mx', permutations) = triangulate mx


triangulate :: Matrix -> (Matrix, Permutations)
triangulate mx = (Mx.fromRows mx', permutations')
  where
    rows = Mx.toRows mx
    permutations = Vec.fromList [0..(length rows - 1)]
    (mx', permutations') = go 0 permutations rows

    go :: Int -> Permutations -> [Vector] -> ([Vector], Permutations)
    go _ ps [] = ([], ps)
    go j ps m@(as:ass)
      | isLastCol = (m, ps)
      | otherwise = (as':ass'', ps'')
      where
        isLastCol = j + 2 >= Vec.length as
        (as':ass', ps') = permuteToMax j ps m
        (ass'', ps'') = go (j + 1) ps' (map updRow ass')

        updRow bs =
          let
            k = bs Vec.! j / as' Vec.! j
            op = \a b -> b - a*k
          in Vec.zipWith op as' bs


permuteToMax :: Int -> Permutations -> [Vector] -> ([Vector], Permutations)
permuteToMax col ps m =
  ( Mx.toRows $ permuteRows 0 i $ permuteCols col j $ Mx.fromRows m
  , swapComponents col j ps
  )
  where
    (i, j, _) = foldr imax (0, 0, 0) $ zip [0..] m

    imax :: (Int, Vec.Vector CReal) -> (Int, Int, CReal) -> (Int, Int, CReal)
    imax (i, v) oldMax@(_, _, max)
        | rowMax > max = (i, j + col, rowMax)
        | otherwise = oldMax
        where (j, rowMax) = Vec.ifoldl vimax (0, 0) (Vec.drop col $ Vec.init v)

    vimax :: (Int, CReal) -> Int -> CReal -> (Int, CReal)
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

nearZero :: CReal -> Bool
nearZero x = abs x < 1e-16

resolveKnown :: Vector -> Vector -> Vector
resolveKnown as xs = Vec.zipWith (*) xs (Vec.tail $ Vec.init as)
