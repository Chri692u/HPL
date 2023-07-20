module Utilities.Logger where

import Prelude hiding (log)
import Control.Monad.Writer

import Math.Matrix

-- | Represents an entry in the logger with a tag (message) and a value.
data Entry = Entry
    { tag :: String -- ^ The tag (message) of the log entry.
    , val :: String -- ^ The value associated with the log entry.
    }

instance Show Entry where
    show (Entry message result) = message ++ " " ++ result

-- | Type synonym for the logger using the Writer monad.
type Logger a = Writer [Entry] a

-- | Runs the logger and returns the result along with the list of log entries.
runLogger :: Logger a -> (a, [Entry])
runLogger = runWriter

-- | Write a log entry with a message and a value.
log :: (Show a) => String -> a -> Logger ()
log msg val = tell [Entry msg (show val)]

-- | Log a note (no associated value).
nb :: String -> Logger ()
nb = log "NB."

-- | Log a calculation (no associated value).
calc :: String -> Logger ()
calc = log "Calculation:"
