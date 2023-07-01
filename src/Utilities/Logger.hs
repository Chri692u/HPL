module Utilities.Logger where

import Prelude hiding (log)
import Control.Monad.Writer

import Math.Matrix

data Entry = Entry
    { tag :: String
    , val :: String
    }

instance Show Entry where
    show (Entry message result) = message ++ " " ++ result

-- Define the Logger type synonym and runLogger using the Writer monad
type Logger a = Writer [Entry] a

runLogger :: Logger a -> (a, [Entry])
runLogger = runWriter

-- Write a log entry with a message and value
log :: (Show a) => String -> a -> Logger ()
log msg val = tell [Entry msg (show val)]

-- Log a note
nb :: String -> Logger ()
nb = log "NB."

-- Log a calculation
calc :: String -> Logger ()
calc = log "Calculation:"