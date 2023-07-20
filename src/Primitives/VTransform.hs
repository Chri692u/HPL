module Primitives.VTransform where

import Data.Array
import Data.List (nub)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Math.Vector

-- | Pick the element at the specified index in the vector.
pick :: Int -> Vector a -> a
pick i v = v ! i

-- | Get the number of elements in the vector.
tally :: Vector a -> Int
tally v = snd $ bounds v

-- | Get the first element of the vector.
vhead :: Vector a -> a
vhead v = v ! 1

-- | Remove the first element from the vector.
behead :: Vector a -> Vector a
behead v = listArray (1, n-1) $ tail $ elems v
  where n = tally v

-- | Reverse the vector.
hat :: Vector a -> Vector a
hat v = listArray (1, tally v) (reverse (elems v))

-- | Get the last element of the vector.
vtail :: Vector a -> a
vtail v = v ! n
    where n = tally v

-- | Remove duplicate elements from the vector while preserving the order.
vnub :: Eq a => Vector a -> Vector a
vnub v = listArray (1, un) us
    where un = length us
          us = nub $ elems v

-- | Encode the elements of the vector using a given base and return a vector of strings.
encode :: (Integral a, Show a) => a -> Vector a -> Vector String
encode b v = listArray (1, tally v) strEncoding
    where strEncoding = map toBase $ elems v
          toBase n = showIntAtBase b intToDigit n ""
