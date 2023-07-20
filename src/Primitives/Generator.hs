module Primitives.Generator where

import Data.Array
import Data.List (nub)
import System.Random

import Math.Matrix
import Math.Vector

-- | Fixed random number generator seed.
rng :: StdGen
rng = mkStdGen 9812389

-- | Generate an iota matrix (a row vector with consecutive integer elements).
iota :: Int -> Matrix Int
iota i = reshape 1 i [1..i]

-- | Generate a matrix with unique random integers in the range [1, max].
deal :: Int -> Int -> Matrix Int
deal i max
    | i > max = error "DEAL ERROR: The number of elements requested is greater than the maximum."
    | otherwise = reshape 1 i randoms
        where randoms = take i $ nub $ randomRs (1, max) rng

-- | Generate a matrix with the first prime numbers arranged in r rows and c columns.
primes :: Int -> Int -> Matrix Int
primes r c = reshape r c $ sieveList 2 (replicate (n + 1) True)
  where
    n = r * c
    sieveList p arr
      | p * p > n = [i | (i, True) <- zip [2..n] arr]
      | arr !! p = sieveList (p + 1) (markMultiples p arr)
      | otherwise = sieveList (p + 1) arr
    markMultiples p arr = [((i `mod` p) /= 0) && val | (i, val) <- zip [0..] arr]
