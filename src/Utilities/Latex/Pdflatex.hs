module Utilities.Latex.Pdflatex where

import System.IO

import Utilities.Latex.Styles
import Utilities.Latex.Templates

testdoc :: Tex
testdoc = makeDoc def ++ content ++ end
    where def = LatexDoc {title = "test", author = "me", style = minimalStyle}
          content = "XDXDXD\n" :: Tex