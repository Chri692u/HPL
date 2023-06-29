module Math.Vector where

import Data.Array

type Vector a = Array Int a

-- Function for binary operators on vectors
binop :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
binop f v1 v2
    | bounds v1 /= bounds v2 = error "VECTOR LENGTH ERROR"
    | otherwise = array (bounds v1) [(i, f (v1 ! i) (v2 ! i)) | i <- indices v1]

-- Vector addition
(+.) :: Num a => Vector a -> Vector a -> Vector a
(+.) = binop (+)

-- Vector subtraction
(-.) :: Num a => Vector a -> Vector a -> Vector a
(-.) = binop (-)

-- Vector multiplication
(*.) :: Num a => Vector a -> Vector a -> Vector a
(*.) = binop (*)

-- dot product of 2 vectors
dot :: Num a => Array Int a -> Array Int a -> a
dot v1 v2
  | bounds v1 /= bounds v2 = error "VECTOR LENGTH ERROR"
  | otherwise = sum [v1 ! i * v2 ! i | i <- indices v1]

-- Projection with normalization
projection :: (Eq a, Fractional a) => Array Int a -> Array Int a -> Array Int a
projection vec onto
  | normSq == 0 = error "Cannot project onto a zero vector."
  | otherwise = listArray (bounds vec) $ map (* scaleFactor) (elems onto)
  where
    dp = vec `dot` onto
    scaleFactor = dp / normSq
    normSq = onto `dot` onto