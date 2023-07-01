module Math.Vector where

import Data.Array

import Math.Matrix

type Vector a = Array Int a
showV :: Show a => Vector a -> String
showV = unlines . map show . elems

-- Convert a vector to a matrix
toMatrix :: Vector a -> Matrix a
toMatrix vec = Matrix { rows = n, cols = 1, elements = e }
    where n = length $ elems vec
          e = array ((1,1), (n,1)) $ zipWith (\i e -> ((i, 1), e)) [1..n] (elems vec)

-- Convert a matrix to a vector
fromMatrix :: Matrix a -> Vector a
fromMatrix matrix
    | cols matrix == 1 = array (1, rows matrix) $ zip [1..] (map (\i -> elements matrix ! (i, 1)) [1..rows matrix])
    | rows matrix == 1 = array (1, cols matrix) $ zip [1..] (map (\i -> elements matrix ! (1, i)) [1..cols matrix])
    | otherwise = error "VECTOR LENGTH ERROR"

-- todo: from list

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

-- reverse a vector
hat :: Vector a -> Vector a
hat vec = listArray (bounds vec) (reverse (elems vec))

-- Projection with normalization
projection :: (Eq a, Fractional a) => Array Int a -> Array Int a -> Array Int a
projection vec onto
  | normSq == 0 = error "Cannot project onto a zero vector."
  | otherwise = listArray (bounds vec) $ map (* scaleFactor) (elems onto)
  where
    dp = vec `dot` onto
    scaleFactor = dp / normSq
    normSq = onto `dot` onto