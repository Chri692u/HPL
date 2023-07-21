module Main where

import TestMatrix
import TestVector

main :: IO ()
main = do
    putStrLn "Running Matrix Tests..."
    matrixTest <- runMatrixTest
    if matrixTest
        then putStrLn "Matrix tests passed :D"
        else putStrLn "Some matrix tests failed :("
    vectorTests <- runVectorTests
    if vectorTests
        then putStrLn "Vector tests passed :D"
        else putStrLn "Some vector tests failed :("