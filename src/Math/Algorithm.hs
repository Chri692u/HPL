module Math.Algorithm where

import Math.Matrix
import Math.Vector
import Data.Array

-- QR decomposition
decompQR :: (Eq a, Floating a, Ord a) => Matrix a -> (Matrix a, Matrix a)
decompQR m = (matQ, matR)
  where
    getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
    columns = [getCol m row | row <- [1..cols m]]
    qs = gramSchmidt columns []
    matQ = orthogonalize m
    matR = upperTriangular $ transpose matQ * m

-- Get the upper triangular matrix
upperTriangular :: Num a => Matrix a -> Matrix a
upperTriangular matrix = matrix { elements = array bounds updatedElements }
  where
    bounds = ((1, 1), (rows matrix, cols matrix))
    updatedElements = [((r, c), if r > c then 0 else get matrix r c) | (r, c) <- range bounds]
    get m r c = elements m ! (r, c)

-- Compute the orthogonalized matrix
orthogonalize :: (Eq a, Floating a) => Matrix a -> Matrix a
orthogonalize m = transpose $ reshape (rows m) (cols m) (concatMap elems qs)
    where getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
          columns = [getCol m row | row <- [1..cols m]]
          qs = gramSchmidt columns []

-- Gram-Schmidt algorithm
gramSchmidt :: (Fractional a, Eq a, Floating a) => [Array Int a] -> [Array Int a] -> [Array Int a]
gramSchmidt [] processed = processed
gramSchmidt (vec:vs) processed = gramSchmidt vs (processed ++ [step])
  where
    step = normalize $ process vec processed
    normalize vector = listArray (bounds vector) $ map (/ norm) (elems vector)
      where norm = sqrt (vector `dot` vector)

-- Gram-Schmidt process algorithm
process :: (Fractional a, Eq a) => Array Int a -> [Array Int a] -> Array Int a
process vs = foldl (\acc q -> acc -. projection vs q) vs