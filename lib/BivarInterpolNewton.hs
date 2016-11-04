module BivarInterpolNewton where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec
import           Library

-- z = f(x, y)
-- h₁ = 𝚫x = const, h₂ = 𝚫y = const
-- 𝚫ₓzᵢⱼ = z_(i+1, j) - zᵢⱼ
-- 𝚫ᵧzᵢⱼ = z_(i, j+1) - zᵢⱼ
-- 𝚫²ₓₓzᵢⱼ = z_(i+2, j) - 2z_(i+1, j) + zᵢⱼ
-- 𝚫²ᵧᵧzᵢⱼ = z_(i, j+2) - 2z_(i, j+1) + zᵢⱼ
-- 𝚫²ₓᵧzᵢⱼ = [z_(i+1, j+1) - z_(i, j + 1)] - [z_(i+1, j) - zᵢⱼ]
-- F(x,y) = z₀₀
--          + (x - x₀)/h₁ * 𝚫ₓz₀₀
--          + (y - y₀)/h₂ * 𝚫ᵧz₀₀
--          + [(x - x₀)(x - x₁)]/(2h²₁) * 𝚫²ₓₓz₀₀
--          + [(x - x₀)(y - y₀)]/(h₁h₂) * 𝚫²ₓᵧz₀₀
--          + [(y - y₀)(y - y₁)]/(2h²₂) * 𝚫²ᵧᵧz₀₀
compute :: Matrix -> Matrix
compute m =
    Mx.fromLists
  $ ( \mx -> (:mx)
    $ Vec.foldl (++) [0]
    $ Vec.imap (\i x -> genArgs x $ xs Vec.! (i+2)) (Vec.slice 1 (Vec.length xs - 3) xs)
    )
  $ ( zipWith (:)
    $ Vec.foldl (++) []
    $ Vec.imap (\i y -> genArgs y $ ys Vec.! (i+2)) (Vec.slice 1 (Vec.length ys - 3) ys)
    )
  $ Mx.toLists
  $ Mx.fromBlocks 0
  $ Mx.toLists
  $ Mx.imap interpolate zss
  where
    xs = Mx.takeRow m 0
    ys = Mx.takeColumn m 0
    zss = Mx.subMatrix (1, 1) (Mx.rows m - 3, Mx.cols m - 3) m
    genArgs x0 x1 = [x | x <- [x0, x0 + (x1 - x0)/10 .. x1], x < x1]

    interpolate :: (Int, Int) -> CReal -> Matrix
    interpolate (i, j) z = Mx.matrix (length xs')
      [ newtonPolynom zss' xyss x y
      | y <- ys', x <- xs'
      ]
      where
        xs' = genArgs xj xjj
        ys' = genArgs yi yii

        zss' =
          [ [    z,              m Mx.! (i+1, j+2),   m Mx.! (i+1, j+3)]
          , [m Mx.! (i+2, j+1),  m Mx.! (i+2, j+2)]
          , [m Mx.! (i+3, j+1)]
          ]

        xj  = m Mx.! (0, j+1)
        yi  = m Mx.! (i+1, 0)
        xjj = m Mx.! (0, j+2)
        yii = m Mx.! (i+2, 0)

        xyss =
          [ [xj, xjj]
          , [yi, yii]
          ]


newtonPolynom :: [[CReal]] -> [[CReal]] -> CReal -> CReal -> CReal
newtonPolynom zss xyss x y = z
  + (x - xj) / hx * dzx
  + (y - yi) / hy * dzy
  + ((x - xj) * (x - xjj)) / (2  * hx*hx) * dzxx
  + ((x - xj) * (y - yi )) / (     hx*hy) * dzxy
  + ((y - yi) * (y - yii)) / (2  * hy*hy) * dzyy
  where
    [ [z,  zj, zjj], [zi, zij], [zii] ] = zss
    [ [xj, xjj], [yi, yii] ] = xyss

    hx = xjj - xj
    hy = yii - yi
    dzx = zj - z
    dzy = zi - z
    dzxx = zjj - 2 * zj + z
    dzyy = zii - 2 * zi + z
    dzxy = (zij - zi) - (zj - z)
