module Main where
    
import Math.Matrix
import Math.Vector
import Utilities.Interactive

-- Main function to run the example
main :: IO ()
main = do
    let m = reshape 2 3 [-1,0,3,4.0]
    let v = fromMatrix $ reshape 1 3 [1,2,3]
    putStrLn "Print of a matrix:"
    print m
    putStrLn "Print of a vector:"
    printV v