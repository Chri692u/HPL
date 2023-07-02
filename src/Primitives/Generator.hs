module Primitives.Generator where

import Data.Array
import Data.List (nub)
import System.Random

import Math.Matrix
import Math.Vector

-- fixed seed
rng :: StdGen
rng = mkStdGen 9812389

-- Index generation for a vector
iota :: Int -> Matrix Int
iota i = reshape 1 i [1..i]

-- deal
deal :: Int -> Int -> Matrix Int
deal i max
    | i > max = error "DEAL ERROR"
    | otherwise = reshape 1 i randoms
        where randoms = take i $ nub $ randomRs (1, max) rng

-- primes
primes :: Int -> Int -> Matrix Int
primes r c = reshape r c$ sieveList 2 (replicate (n + 1) True)
  where
    n = r * c
    sieveList p arr
      | p * p > n = [i | (i, True) <- zip [2..n] arr]
      | arr !! p = sieveList (p + 1) (markMultiples p arr)
      | otherwise = sieveList (p + 1) arr
    markMultiples p arr = [((i `mod` p) /= 0) && val | (i, val) <- zip [0..] arr]
