module Tokenizer (
    tokenize
) where

import Data.List.Split

dropBlanksNoCondense :: Splitter a -> Splitter a
dropBlanksNoCondense = dropInnerBlanks . dropInitBlank . dropFinalBlank

whitespaceSplit :: String -> [String]
whitespaceSplit = split (dropBlanksNoCondense . dropDelims $ oneOf " \t\n")

parenSplit :: String -> [String]
parenSplit = split (dropBlanksNoCondense $ oneOf "()")

-- TODO: This is dumb and won't work with string literals in source code.
tokenize :: String -> [String]
tokenize source =
    concat . map parenSplit $ whitespaceSplit source
