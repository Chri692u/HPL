module Utilities.Latex.Styles where

data LatexDoc = LatexDoc
    { title :: String
    , author :: String
    , style :: LatexStyle
}

data FontSize = Small | Medium | Large | Huge

data TexVector = OneSize | Later

data TexMatrix = OneSizeM | LaterM

data LatexStyle = LatexStyle
    { sectionSize :: FontSize
    , bodySize :: FontSize
    , twoColumns :: Bool
    , vectorStyle :: TexVector
    , matrixStyle :: TexMatrix
    }

minimalStyle :: LatexStyle
minimalStyle = LatexStyle
    { sectionSize = Large
    , bodySize = Medium
    , twoColumns = False
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }

twoColumnStyle :: LatexStyle
twoColumnStyle = LatexStyle
    { sectionSize = Medium
    , bodySize = Medium
    , twoColumns = True
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }

compactStyle :: LatexStyle
compactStyle = LatexStyle
    { sectionSize = Small
    , bodySize = Medium
    , twoColumns = True
    , vectorStyle = OneSize
    , matrixStyle = OneSizeM
    }