module Utilities.Latex.Templates where

import Utilities.Latex.Styles

-- | Type synonym for LaTeX text.
type Tex = String

-- | LaTeX command to set the font family to sans-serif.
family :: Tex
family = "\\renewcommand{\\familydefault}{\\sfdefault} % Sans-serif font family"

-- | LaTeX command to end the document.
end :: Tex
end = "\\end{document}"
