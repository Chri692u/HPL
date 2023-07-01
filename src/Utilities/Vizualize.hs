module Utilities.Vizualize where

import Data.Array

import Math.Matrix
import Math.Vector

-- Print a vector
printV :: (Show a) => Vector a -> IO ()
printV = putStrLn . showV