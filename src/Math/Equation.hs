module Math.Equation where

import Data.Array
import Math.Matrix
import Math.Algorithms
import Math.Vector

type Solution a = Vector a

-- Solve function
solve :: (Fractional a, Eq a) => Matrix a -> Vector a -> Solution a
solve m = backwards (asRows m)

-- Solve the equation system by backwards propagation
backwards :: (Eq a, Fractional a) =>Array Int (Row a) -> Row a -> Solution a
backwards m b = sol
  where
    n = snd $ bounds b
    sol = array (1,n) solution
    solution = [(check i m, (b ! i - sum [sol ! j * (m ! i ! j) | j <- [i+1..n]]) / m ! i ! i) | i <- [n,n-1..1]]

-- Check if a solution is impossible
check :: (Eq a, Num a) => Int -> Array Int (Row a) -> Int
check i m = if m ! i ! i == 0 then error "Unsolvable equation system" else i