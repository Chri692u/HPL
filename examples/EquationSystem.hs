module Main where

import Math.Matrix
import Math.Vector
import Math.Algorithms
import Math.Equation
import Utilities.Vizualize

-- Main function to run the example
main :: IO ()
main = do
    let a = reshape 2 2 [1, 1, 1, 6]
        b = fromMatrix $ reshape 2 1 [-3,2]
        (q,r) = decompQR a
        solution = solve r b
    putStrLn "Solving linear system:"
    print a
    putStrLn "With right hand side vector:"
    printV b
    putStrLn "QR decomposition:"
    putStrLn "Q:"
    print q
    putStrLn "R:"
    print r
    putStrLn "Solution vector from solving R x = b:"
    printV solution