module BodeShooting where

import qualified Data.Matrix    as M
import qualified Data.Vector    as V
import           Function
import           Library
import           SodeRungeKutta


compute :: (Vector, FMatrix, Matrix) -> Either ComputeError Matrix
compute (domain, equations, bounds) = do
  eta3' <- eta3
  integrate eta3' (zeta eta3')
  where
    [[a, b, c ], [d, e, f ]] = M.toLists equations
    [[p, q, r1], [s, t, r2]] = M.toLists bounds

    integrate eta zeta = SodeRungeKutta.compute (domain V.! 1 / 100, M.fromLists
      [ [ Function "a(x0)x1 + b(x0)x2 + c(x0)" (linearFunction a b c)
        , Function "const" (Right . const eta)
        ]
      , [ Function "a(x0)x1 + b(x0)x2 + c(x0)" (linearFunction d e f)
        , Function "const" (Right . const zeta)
        ]
      ])

    eta1 = domain V.! 0
    eta2 = domain V.! 1
    eta3 = do
      psi_eta1 <- psi eta1
      psi_eta2 <- psi eta2
      return $ eta2 - (eta2 - eta1) * (psi_eta2) / (psi_eta2 - psi_eta1)

    zeta eta = (r1 - p * eta) / q

    psi eta = do
      uvs <- integrate eta (zeta eta)
      let [_, u, v] = V.toList $ last $ M.toRows $ uvs
      return $ s * u + t * v - r2


linearFunction a b c v = do
  ax <- runFunction a v
  bx <- runFunction b v
  cx <- runFunction c v
  return $ ax * (v V.! 1) + bx * (v V.! 2) + cx
