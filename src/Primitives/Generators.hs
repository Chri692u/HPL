module Primitives.Generators where

import Data.Array
import Math.Vector

-- Index generation for a vector
iota :: Int -> Vector Int
iota i = listArray (1, i) [1..i]

