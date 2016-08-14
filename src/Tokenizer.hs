-- |A custom tokenizer that converts a string of hasp source code to a list of
-- individual tokens, which can then be fed into the parser.

module Tokenizer
( tokenize
, Token
) where

import Error

type Token = String

tokenize :: String -> Either Error [Token]
tokenize = genTokenSeq False [] ""

keepDelim :: [Token] -> Token -> Char -> String -> Either Error [String]
keepDelim acc "" char nextInput =
    genTokenSeq False (acc ++ [[char]]) "" nextInput
keepDelim acc token char nextInput =
    genTokenSeq False (acc ++ [token, [char]]) "" nextInput

dropDelim :: [Token] -> Token -> Char -> String -> Either Error [String]
dropDelim acc "" char nextInput =
    genTokenSeq False acc "" nextInput
dropDelim acc token char nextInput =
    genTokenSeq False (acc ++ [token]) "" nextInput

appendChar :: Bool -> [Token] -> Token -> Char -> String ->
    Either Error [String]
appendChar isString acc token char nextInput =
    genTokenSeq isString acc (token ++ [char]) nextInput

startString :: [Token] -> Token -> Char -> String -> Either Error [String]
startString acc "" char nextInput =
    genTokenSeq True acc [char] nextInput
startString acc token char nextInput =
    genTokenSeq True (acc ++ [token]) [char] nextInput

endString :: [Token] -> Token -> Char -> String -> Either Error [String]
endString acc token curChar nextInput =
    genTokenSeq False (acc ++ [token ++ [curChar]]) "" nextInput

-- TODO: Since we are appending to the end, see if there is a more efficient
-- way to do that.
genTokenSeq :: Bool -> [Token] -> Token -> String -> Either Error [String]
genTokenSeq False acc "" "" = Right acc
genTokenSeq False acc token "" = Right (acc ++ [token])

genTokenSeq True acc token "" = Left (SyntaxError "Unclosed string literal")

genTokenSeq False acc token (curChar:nextInput)
    | curChar `elem` "()"    = keepDelim acc token curChar nextInput
    | curChar `elem` " \n\t" = dropDelim acc token curChar nextInput
    | curChar == '"'         = startString acc token curChar nextInput
    | otherwise              = appendChar False acc token curChar nextInput

genTokenSeq True acc token (curChar:nextInput)
    | curChar == '"' = endString acc token curChar nextInput
    | otherwise      = appendChar True acc token curChar nextInput
