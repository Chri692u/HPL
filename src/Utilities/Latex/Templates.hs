module Utilities.Latex.Templates where

import Utilities.Latex.Styles

type Tex = String

family :: Tex
family = "\\renewcommand{\\familydefault}{\\sfdefault} % Sans-serif font family"

end :: Tex
end = "\\end{document}"