module Main where

import Prelude hiding (log)
import Matrix
import Logger

-- Example
calculation :: Logger ()
calculation = do
    let zeros = reshape 2 2 [0]
    nb "a null matrix"
    log "zeros" zeros

    let ones = reshape 2 2 [1]
    nb "an all-1 matrix"
    log "ones" ones

    let mixed = reshape 2 2 [-1,0,3,4.0]
    nb "a matrix with positive, negative and floating point numbers"
    log "mixed" mixed

    let id = reshape 2 2 [1, 0, 0, 1]
    nb "a 2x2 identity matrix"
    log "id" id

    let res1 = shape id
    log "shape of res:" res1

    -- Addition
    let res2 = id + zeros
    log "id + zeros:" res2

    -- Multiplication
    let res3 = id * ones
    log "id * ones:" res3

    -- Scalar to 1x1 matrix
    let five = 5 :: Matrix Int
    log "scalar to 1x1:" five

    -- Negation
    log "negation:" $ negate ones

    -- Absolute values of each entry
    log "abs:" $ abs mixed

    -- Signum of each entry
    log "signum" $ signum mixed


-- Main function to run the example
main :: IO ()
main = do
    let (_, logs) = runLogger calculation
    mapM_ print logs