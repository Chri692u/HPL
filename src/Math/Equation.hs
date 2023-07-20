module Math.Equation where

import Data.Array
import Math.Matrix
import Math.Algorithms
import Math.Vector

-- | Type synonym for the solution of an equation system, represented as a vector.
type Solution a = Vector a

-- | Solve an equation system represented by a matrix and a vector.
solve :: (Fractional a, Eq a) => Matrix a -> Vector a -> Solution a
solve m = backwards (asRows m)

-- | Solve the equation system by backward substitution.
backwards :: (Eq a, Fractional a) => Array Int (Row a) -> Row a -> Solution a
backwards m b = sol
  where
    n = snd $ bounds b
    sol = array (1, n) solution
    solution = [(check i m, (b ! i - sum [sol ! j * (m ! i ! j) | j <- [i + 1..n]]) / m ! i ! i) | i <- [n, n - 1..1]]

-- | Check if a solution is impossible due to a zero pivot element.
check :: (Eq a, Num a) => Int -> Array Int (Row a) -> Int
check i m = if m ! i ! i == 0 then error "Unsolvable equation system: Zero pivot element" else i
