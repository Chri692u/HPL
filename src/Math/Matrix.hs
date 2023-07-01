{-# LANGUAGE InstanceSigs #-}

module Math.Matrix where

import Data.Array
import Data.Array.Base(amap)
import Data.List

type Row a = (Array Int a)

data Matrix a = Matrix
    { rows :: Int
    , cols :: Int
    , elements :: Array (Int, Int) a
    }

instance (Show a) => Show (Matrix a) where
    show :: (Show a) => Matrix a -> String
    show matrix =
        let m = elements matrix
            ((r1, c1), (rn, cn)) = bounds m
        in unlines
            [ intercalate "   " [show $ m ! (r, c) | c <- [c1 .. cn]]
            | r <- [r1 .. rn]
            ]

instance Functor Matrix where
    fmap :: (a -> b) -> Matrix a -> Matrix b
    fmap f matrix = matrix { elements = fmap f (elements matrix) }

instance Eq a => Eq (Matrix a) where
    (==) :: Eq a => Matrix a -> Matrix a -> Bool
    m1 == m2 = shapesOk && valuesOk
        where   shapesOk = shape m1 == shape m2
                valuesOk = and $ zipWith (==) l1 l2
                l1 = toList m1
                l2 = toList m2

instance Num a => Num (Matrix a) where
    (+) :: Num a => Matrix a -> Matrix a -> Matrix a
    m1 + m2
        | shape m1 /= shape m2 = error "DIMENSION ERROR" zipWith (+) l1 l2
        | otherwise = reshape r c $ zipWith (+) l1 l2
            where r = rows m1
                  c = cols m1
                  l1 = toList m1
                  l2 = toList m2

    (*) :: Num a => Matrix a -> Matrix a -> Matrix a
    m1 * m2 = innerProduct (+) (*) m1 m2

    fromInteger :: Num a => Integer -> Matrix a
    fromInteger n = Matrix 1 1 (array (ones, ones) [(ones, fromInteger n)])
        where ones = (1,1)

    negate :: Num a => Matrix a -> Matrix a
    negate = fmap negate

    abs :: Num a => Matrix a -> Matrix a
    abs = fmap abs

    signum :: Num a => Matrix a -> Matrix a
    signum = fmap signum

instance Semigroup (Matrix a) where
    (<>) :: Matrix a -> Matrix a -> Matrix a
    m1 <> m2
        | rows m1 /= rows m2 = error "DIMENSION ERROR"
        | otherwise = reshape (rows m1 + cols m2) (cols m1) $ concat $ allRows1 ++ allRows2
        where
            getRow m r = [get m r c | c <- [1..cols m]]
            allRows1 = [getRow m1 row1 | row1 <- [1..rows m1]]
            allRows2 = [getRow m2 row2 | row2 <- [1..1+rows m2]]

-- Lookup the value at a specific row and column
get :: Matrix a -> Int -> Int -> a
get matrix row col = elements matrix ! (row, col)

-- Convert a matrix to an array of rows
asRows :: Matrix a -> Array Int (Row a)
asRows m = array (1, ncols) [(i, col i) | i <- [1..ncols]]
    where m' = elements m
          ncols = cols m
          nrows = rows m
          col i = array (1, nrows) [(j, m' ! (j, i)) | j <- [1..nrows]]

-- Convert a matrix to a list
toList :: Matrix a -> [a]
toList matrix = [get matrix r c | r <- [1..rows matrix], c <- [1..cols matrix]]

-- Primitives
-- Return the shape of the matrix
shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

-- Return a matrix of shape rows x cols and elements from values (wrapping)
reshape :: Int -> Int -> [a] -> Matrix a
reshape rows cols vs =
    let n = rows * cols
        values = take n (cycle vs)
        indexed = zipWith (\i v -> ((i `div` cols + 1, i `mod` cols + 1), v)) [0..] values
        bounds = ((1, 1), (rows, cols))
        arr = array bounds indexed
    in Matrix rows cols arr

-- Inner product algorithm
innerProduct :: Num a => (a -> a -> a) -> (b -> c -> a) -> Matrix b -> Matrix c -> Matrix a
innerProduct f g m1 m2
    | cols m1 /= rows m2 = error "SIZE ERROR: Incompatible sizes for inner product"
    | otherwise = reshape (rows m1) (cols m2) [ getList r c | r <- [1..rows m1], c <- [1..cols m2]]
    where
        getList r c = foldl1 f (zipWith g (getRow m1 r) (getCol m2 c))
        getRow m r = [get m r c | c <- [1..cols m]]
        getCol m c = [get m r c | r <- [1..rows m]]

-- Transpose a matrix
transpose :: Matrix a -> Matrix a
transpose matrix = reshape (cols matrix) (rows matrix) $ transpose' matrix
  where
    transpose' m = [get m c r | r <- [1..cols m], c <- [1..rows m]]