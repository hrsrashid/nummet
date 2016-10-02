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
        (as':ass', ps') = permuteIfZero j ps m
        (ass'', ps'') = go (j + 1) ps' (map updRow ass')

        updRow bs =
          let
            k = bs Vec.! j / as' Vec.! j
            op = \a b -> b - a*k
          in Vec.zipWith op as' bs


permuteIfZero :: Int -> Permutations -> [Vector] -> ([Vector], Permutations)
permuteIfZero j ps m = if nearZero (as Vec.! j) then goCols (j + 1) j ps m else res
  where
    res@(as:_, _) = goRows 1 j ps m
    err = error "Can't permute elements of empty matrix"

    goRows _ _ _ [] = err
    goRows i j ps m@(as:_)
      | isLastRow = (m, ps)
      | nearZero (as Vec.! j) = goRows (i + 1) j ps permuted
      | otherwise = (m, ps)
      where
        isLastRow = i + 2 >= Vec.length as
        permuted = Mx.toRows $ permuteRows 0 i $ Mx.fromRows m

    goCols _ _ _ [] = err
    goCols i j ps m@(as:_)
      | isLastCol = (m, ps)
      | nearZero (as Vec.! j) = goCols (i + 1) j ps' permuted
      | otherwise = (m, ps)
      where
        isLastCol = i + 2 >= Vec.length as
        permuted = Mx.toRows $ permuteCols j i $ Mx.fromRows m
        ps' = swapComponents j i ps


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
