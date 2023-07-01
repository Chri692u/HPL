module Utilities.Interactive where

import Data.Array

import Math.Matrix
import Math.Vector

-- Print a matrix

-- Print a list as a vector
printV :: (Show a) => Vector a -> IO ()
printV = putStrLn . showV