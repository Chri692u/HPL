module Main where

import Data.IORef
import TestMatrix
import TestVector

incr :: Num a => IORef a -> IO ()
incr c = modifyIORef' c (+1)

main :: IO ()
main = do
    propteries <- newIORef (0 :: Integer)
    putStrLn "Running property tests..."
    matrixTest <- runMatrixTest
    if matrixTest
        then do 
            incr propteries
            putStrLn "Matrix tests passed :D"
        else putStrLn "Some matrix tests failed :("
    vectorTests <- runVectorTests
    if vectorTests
        then do
            incr propteries 
            putStrLn "Vector tests passed :D"
        else putStrLn "Some vector tests failed :("

    passed <- readIORef propteries
    putStrLn $ "Total test files passed: " ++ show passed