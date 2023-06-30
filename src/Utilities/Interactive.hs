module Utilities.Interactive where

import Data.Array

import Math.Matrix
import Math.Vector

-- Print a matrix
printM :: Show a => Matrix a -> IO ()
printM m = do
    let vecs = map elems list
    let padding = 2 + length (concatMap (\x -> " " ++ show x ++ " ") $ head vecs)
    putStrLn $ replicate padding '-'
    mapM_ printV vecs
    putStrLn $ replicate padding '-'
    where getCol m c = listArray (1, rows m) [get m r c | r <- [1..rows m]]
          list = [getCol m row | row <- [1..cols m]]

-- Print a list as a vector
printV :: (Show a) => [a] -> IO ()
printV v = do
    let line = concatMap (\x -> " " ++ show x ++ " ") v
    putStrLn $ "|" ++ line ++ "|"