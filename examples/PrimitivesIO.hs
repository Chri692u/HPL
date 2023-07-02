module Main where

import Math.Matrix
import Math.Vector
import Primitives.Generator
import Primitives.VTransform
import Utilities.Vizualize

-- Main function to run the example
main :: IO ()
main = do
    putStrLn "Matrix generators:"
    putStr $ "iota 5: " ++ show (iota 5)
    putStr $ "deal 5 10: " ++ show (deal 5 10)
    putStr $ "primes 1 7: " ++ show (primes 1 7)
    putStrLn ""
    putStrLn "Vector transformations:"
    let viota = fromMatrix $ iota 5
    putStrLn $ "Vector from list: " ++ showV (fromList [1,2,3])
    putStrLn $ "pick 5 (iota 5): " ++ show (pick 5 viota)
    putStrLn $ "tally (iota 5): " ++ show (tally viota)
    putStrLn $ "vhead (iota 5): " ++ show (tally viota)
    putStrLn $ "behead (iota 5): " ++ showV (behead viota)
    putStrLn $ "vtail (iota 5): " ++ show (vtail viota)
    putStrLn $ "concat (iota 2 <.> iota 2): " ++ show (fromMatrix (iota 2) <.> fromMatrix (iota 2))
    putStrLn $ "nub of (iota 2 <.> iota 2):" ++ showV (vnub (fromMatrix (iota 2) <.> fromMatrix (iota 2)))
    putStrLn $ "encode 2 (iota 5):" ++ showV (encode 2 $ fromMatrix (iota 5))