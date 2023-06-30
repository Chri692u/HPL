module Math.Equation where

import Data.Array
import Math.Matrix
import Math.Vector
import Math.Algorithms

type Solution a = Vector a

data LinearEquation a = LinearEquation
    { coefficient :: Matrix a
    , rhs :: Vector a
    }

-- Backward substitution for solving upper triangular system
solve :: Fractional a => LinearEquation a -> Vector a
solve eq = listArray (1, n) $ reverse $ solveRow (reverse $ elems vec) n
  where
    matR = coefficient eq
    vec = rhs eq
    n = rows matR
    solveRow [b] 1 = [b / get matR 1 1]
    solveRow (b:bs) k = xk : solveRow bs (k - 1)
      where
        xk = (b - sum [get matR k j * xj | (j, xj) <- zip [k + 1..n] (solveRow bs (k + 1))]) / get matR k k
