-- |A custom tokenizer that converts a string of hasp source code to a list of
-- individual tokens, which can then be fed into the parser.

module Tokenizer
( tokenize
, Token
) where

import Error

type Token = String

tokenize :: String -> ThrowsError [Token]
tokenize = genTokenSeq False [] ""

keepDelim :: [Token] -> Token -> Char -> String -> ThrowsError [String]
keepDelim acc "" char = genTokenSeq False (acc ++ [[char]]) ""
keepDelim acc token char = genTokenSeq False (acc ++ [token, [char]]) ""

dropDelim :: [Token] -> Token -> Char -> String -> ThrowsError [String]
dropDelim acc "" char = genTokenSeq False acc "" 
dropDelim acc token char = genTokenSeq False (acc ++ [token]) ""

appendChar :: Bool -> [Token] -> Token -> Char -> String ->
    ThrowsError [String]
appendChar isString acc token char = genTokenSeq isString acc (token ++ [char])

startString :: [Token] -> Token -> Char -> String -> ThrowsError [String]
startString acc "" char = genTokenSeq True acc [char]
startString acc token char = genTokenSeq True (acc ++ [token]) [char]

endString :: [Token] -> Token -> Char -> String -> ThrowsError [String]
endString acc token curChar =
    genTokenSeq False (acc ++ [token ++ [curChar]]) ""

-- TODO: Since we are appending to the end, see if there is a more efficient
-- way to do that.
genTokenSeq :: Bool -> [Token] -> Token -> String -> ThrowsError [String]
genTokenSeq False acc "" "" = return acc
genTokenSeq False acc token "" = return (acc ++ [token])

genTokenSeq True acc token "" = throw $ SyntaxError "Unclosed string literal"

genTokenSeq False acc token (curChar:nextInput)
    | curChar `elem` "()"    = keepDelim acc token curChar nextInput
    | curChar `elem` " \n\t" = dropDelim acc token curChar nextInput
    | curChar == '"'         = startString acc token curChar nextInput
    | otherwise              = appendChar False acc token curChar nextInput

genTokenSeq True acc token (curChar:nextInput)
    | curChar == '"' = endString acc token curChar nextInput
    | otherwise      = appendChar True acc token curChar nextInput
