module SlaeGauss where

import Data.Number.CReal
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx

type Matrix = Mx.Matrix CReal
type Vector = Vec.Vector CReal

compute :: Matrix -> Vector
compute = backtrack . triangulate

triangulate :: Matrix -> Matrix
triangulate = Mx.fromRows . go 0 . Mx.toRows
  where
    go :: Int -> [Vector] -> [Vector]
    go _ [] = []
    go j m@(as:ass)
      | j + 2 >= Vec.length as = m
      | otherwise = as : go (j + 1) (map updRow ass)
      where
        updRow bs =
          let
            k = bs Vec.! j / as Vec.! j
            op = \a b -> b - a*k
          in Vec.zipWith op as bs

backtrack :: Matrix -> Vector
backtrack = foldr eqSolver Vec.empty . Mx.toRows

eqSolver :: Vector -> Vector -> Vector
eqSolver as xs = Vec.cons ((Vec.last as' - Vec.sum (resolveKnown as' xs)) / Vec.head as') xs
  where as' = Vec.dropWhile nearZero as

nearZero :: CReal -> Bool
nearZero x = abs x < 1e-16

resolveKnown :: Vector -> Vector -> Vector
resolveKnown as xs = Vec.zipWith (*) xs (Vec.tail $ Vec.init as)
