module Utilities.Latex.Pdflatex where

import System.IO

import Utilities.Latex.Styles
import Utilities.Latex.Templates

-- | Get the document class with optional two-column format.
getClassCols :: Bool -> Tex
getClassCols True = "\\documentclass[twocolumn]{article}\n"
getClassCols False = "\\documentclass{article}\n"

-- | Create font settings based on the specified 'FontSize'.
makeFonts :: FontSize -> Tex
makeFonts Small = "\\sectionfont{\\small} % Section titles in small font size"
makeFonts Large = "\\sectionfont{\\large} % Section titles in large font size"
makeFonts Huge = "\\sectionfont{\\huge} % Section titles in huge font size"

-- | Generate the LaTeX document based on the 'LatexDoc' configuration.
makeDoc :: LatexDoc -> Tex
makeDoc doc = dclass ++ dtitle ++ today ++ by ++ begin ++ section
    where dclass = getClassCols (twoColumns $ style doc)
          dtitle = "\\title{" ++ title doc ++ "}\n"
          today = "\\date{\\today}\n"
          by = "\\author{" ++ author doc ++ "}\n"
          begin = "\\begin{document}\n"
          make = "\\maketitle\n"
          section = "\\section*{Log of " ++ title doc  ++ ":}\n"

testdoc :: Tex
testdoc = makeDoc def ++ content ++ end
    where def = LatexDoc {title = "test", author = "me", style = minimalStyle}
          content = "XDXDXD\n" :: Tex
