module Main where

import Data.Array
import Math.Matrix
import Math.Algorithm

main :: IO ()
main = do
    putStrLn "Solving system:"
    let m = reshape 2 2 [5,-4,1,2]
    print m
    let (q,r) = decompQR m
    putStrLn "Q:"
    print q
    putStrLn "R:"
    print r