module Primitives.VTransform where

import Data.Array
import Data.List(nub)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Math.Vector

-- pick
pick :: Int -> Vector a -> a
pick i v = v ! i

-- tally
tally :: Vector a -> Int
tally v = snd $ bounds v

-- vhead
vhead :: Vector a -> a
vhead v = v ! 1

-- behead
behead :: Vector a -> Vector a
behead v = listArray (1, n-1) $ tail $ elems v
  where n = tally v

-- reverse a vector
hat :: Vector a -> Vector a
hat v = listArray (1, tally v) (reverse (elems v))

-- vtail
vtail :: Vector a -> a
vtail v = v ! n
    where n = tally v

-- nub
vnub :: Eq a => Vector a -> Vector a
vnub v = listArray (1, un) us
    where un = length us
          us = nub $ elems v

encode :: (Integral a, Show a) => a -> Vector a -> Vector String
encode b v = listArray (1, tally v) strEncoding
    where strEncoding = map toBase $ elems v
          toBase n = showIntAtBase b intToDigit n ""

-- decode