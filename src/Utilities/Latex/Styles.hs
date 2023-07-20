module Utilities.Latex.Styles where

-- | Represents a LaTeX document configuration.
data LatexDoc = LatexDoc
    { title :: String          -- ^ Title of the document.
    , author :: String         -- ^ Author of the document.
    , style :: LatexStyle      -- ^ LaTeX style configuration.
    }

-- | Represents the style configuration for a LaTeX document.
data LatexStyle = LatexStyle
    { sectionSize :: FontSize      -- ^ Font size for section titles.
    , bodySize :: FontSize         -- ^ Font size for the body text.
    , twoColumns :: Bool           -- ^ Whether the document uses two columns layout.
    , vectorStyle :: TexVector     -- ^ Style for vectors in the document.
    , matrixStyle :: TexMatrix     -- ^ Style for matrices in the document.
    }

-- | Represents different font sizes available for LaTeX documents.
data FontSize = Small | Medium | Large | Huge

-- | Represents the style configuration for vectors in the document.
data TexVector = OneSize | Later

-- | Represents the style configuration for matrices in the document.
data TexMatrix = OneSizeM | LaterM

-- | Predefined LaTeX style with larger section titles and medium body text.
minimalStyle :: LatexStyle
minimalStyle = LatexStyle
    { sectionSize = Large
    , bodySize = Medium
    , twoColumns = False
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }

-- | Predefined LaTeX style with medium section titles, medium body text, and two columns layout.
twoColumnStyle :: LatexStyle
twoColumnStyle = LatexStyle
    { sectionSize = Medium
    , bodySize = Medium
    , twoColumns = True
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }

-- | Predefined LaTeX style with smaller section titles, medium body text, and two columns layout.
compactStyle :: LatexStyle
compactStyle = LatexStyle
    { sectionSize = Small
    , bodySize = Medium
    , twoColumns = True
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }
