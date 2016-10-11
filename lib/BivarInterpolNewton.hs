module BivarInterpolNewton where

import qualified Data.Matrix       as Mx
import           Data.Number.CReal
import qualified Data.Vector       as Vec
import           Library


compute :: Matrix -> Matrix
compute m = Mx.fromRows rows
  where
    rows = Mx.toRows m
    hs = argDeltas rows


argDeltas :: [Vector] -> Vec.Vector (CReal, CReal)
argDeltas [] = Vec.empty
argDeltas rows@(_:restRows) = Vec.fromList $ zipWith getArgDeltas rows restRows
  where
    getArgDeltas prev curr =
      ( curr Vec.! 0 - prev Vec.! 0
      , curr Vec.! 1 - prev Vec.! 1
      )


newtonPolynom :: CReal -> CReal -> CReal
newtonPolynom x y = x * y
