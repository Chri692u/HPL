module Utilities.Latex.Templates where

import Utilities.Latex.Styles

type Tex = String

getClassCols :: Bool -> Tex
getClassCols True = "\\documentclass[twocolumn]{article}\n"
getClassCols False = "\\documentclass{article}\n"

family :: Tex
family = "\\renewcommand{\\familydefault}{\\sfdefault} % Sans-serif font family"

end :: Tex
end = "\\end{document}"

makeFonts :: FontSize -> Tex
makeFonts Small = "\\sectionfont{\\small} % Section titles in small font size"
makeFonts Large = "\\sectionfont{\\large} % Section titles in small font size"
makeFonts Huge = "\\sectionfont{\\huge} % Section titles in small font size"

makeDoc :: LatexDoc -> Tex
makeDoc doc = dclass ++ dtitle ++ today ++ by ++ begin ++ section
    where dclass = getClassCols (twoColumns $ style doc)
          dtitle = "\\title{" ++ title doc ++ "}\n"
          today = "\\date{\today}\n"
          by = "\\author{" ++ author doc ++ "}\n"
          begin = "\\begin{document}\n"
          make = "\\maketitle\n"
          section = "\\section*{Log of " ++ title doc  ++ ":}\n"