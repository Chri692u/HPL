module Main where

import Math.Matrix
import Math.Vector
import Primitives.Generators
import Utilities.Interactive

-- Main function to run the example
main :: IO ()
main = do
    putStrLn "Reshape a list into a matrix:"
    let m = reshape 2 3 [-1,0,3,4.0]
    print m
    putStrLn "Convert a reshape to a vector:"
    let v = fromMatrix $ reshape 1 3 [9,15,3]
    printV v
    putStrLn "index generator for vectors:"
    print $ toMatrix $ iota 5
    putStrLn "time table - (iota 9) +.* iota 9:"
    putStrLn "TODO"