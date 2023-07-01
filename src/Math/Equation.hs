module Math.Equation where

import Data.Array
import Math.Matrix
import Math.Algorithms
import Math.Vector

type Solution a = Vector a
type Row a = (Array Int a)

solve :: (Num a, Fractional a) => Matrix a -> Vector a -> Solution a
solve m = backwards (asRows m)

backwards :: (Num a, Fractional a) => Array Int (Row a) -> Vector a -> Solution a
backwards m b = sol
  where
    n = snd $ bounds b
    sol = array (1,n)
      [(i,
        (b ! i - sum [sol ! j * (m ! i ! j) | j <- [i+1..n]]) /
          m ! i ! i
      ) | i <- [n,n-1..1]]

asRows :: Matrix a -> Array Int (Row a)
asRows m = array (1, ncols) [(i, col i) | i <- [1..ncols]]
    where m' = elements m
          ncols = cols m
          nrows = rows m
          col i = array (1, nrows) [(j, m' ! (j, i)) | j <- [1..nrows]]