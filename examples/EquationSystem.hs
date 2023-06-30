module Main where

import Data.Array
import Math.Matrix
import Math.Vector
import Math.Algorithms
import Math.Equation

main :: IO ()
main = do
    putStrLn "Solving system:"
    let coefficents = reshape 3 3 [1,1,0,1,0,1,0,1,1]
    let b = fromMatrix $ reshape 1 3 [1,-2,3]
    let (q,r) = decompQR coefficents
    let solution = solve (LinearEquation q b)
    print $ elems solution