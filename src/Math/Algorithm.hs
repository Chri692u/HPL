module Math.Algorithm where

import Math.Matrix
import Math.Vector
import Data.Array

decompQR :: (Eq a, Floating a) => Matrix a -> (Matrix a, Matrix a)
decompQR m = (matQ, matQ)
  where
    getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
    columns = [getCol m row | row <- [1..cols m]]
    qs = gramSchmidt columns []
    matQ = orthogonalize m
    --matR = innerProduct (+) (*) matQ m

orthogonalize :: (Eq a, Floating a) => Matrix a -> Matrix a
orthogonalize m = transpose $ reshape (rows m) (cols m) (concatMap elems qs)
    where getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
          columns = [getCol m row | row <- [1..cols m]]
          qs = gramSchmidt columns []

gramSchmidt :: (Fractional a, Eq a, Floating a) => [Array Int a] -> [Array Int a] -> [Array Int a]
gramSchmidt [] processed = processed
gramSchmidt (vec:vs) processed = gramSchmidt vs (processed ++ [step])
  where
    step = normalize $ process vec processed
    normalize vector = listArray (bounds vector) $ map (/ norm) (elems vector)
      where norm = sqrt (vector `dot` vector)

process :: (Fractional a, Eq a) => Array Int a -> [Array Int a] -> Array Int a
process vs = foldl (\acc q -> acc -. projection vs q) vs