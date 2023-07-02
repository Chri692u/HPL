module Main where

import Prelude hiding (log)
import Math.Matrix
import Utilities.Latex.Pdflatex

import System.IO

main :: IO ()
main = do
  let filePath = "C:\\Users\\chri6\\OneDrive\\Skrivebord\\Github\\HPL\\test.tex"
  writeFile filePath testdoc