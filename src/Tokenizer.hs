-- |A custom tokenizer that converts a string of hasp source code to a list of
-- individual tokens, which can then be fed into the parser.

module Tokenizer
( tokenize
, Token
) where

import Data.Sequence
import Data.Foldable (toList)
import Control.Monad (liftM)

import Error

type Token = String

tokenize :: String -> ThrowsError [Token]
tokenize source = liftM toList $ genTokenSeq False empty "" source

keepDelim :: Seq Token -> Token -> Char -> String -> ThrowsError (Seq Token)
keepDelim acc "" char = genTokenSeq False (acc |> [char]) ""
keepDelim acc token char = genTokenSeq False (acc |> token |> [char]) ""

dropDelim :: Seq Token -> Token -> Char -> String -> ThrowsError (Seq Token)
dropDelim acc "" char = genTokenSeq False acc "" 
dropDelim acc token char = genTokenSeq False (acc |> token) ""

appendChar :: Bool -> Seq Token -> Token -> Char -> String ->
    ThrowsError (Seq Token)
appendChar isString acc token char = genTokenSeq isString acc (token ++ [char])

startString :: Seq Token -> Token -> Char -> String -> ThrowsError (Seq Token)
startString acc "" char = genTokenSeq True acc [char]
startString acc token char = genTokenSeq True (acc |> token) [char]

endString :: Seq Token -> Token -> Char -> String -> ThrowsError (Seq Token)
endString acc token curChar =
    genTokenSeq False (acc |> (token ++ [curChar])) ""

genTokenSeq :: Bool -> Seq Token -> Token -> String -> ThrowsError (Seq Token)
genTokenSeq False acc "" "" = return acc
genTokenSeq False acc token "" = return (acc |> token)

genTokenSeq True acc token "" = throw $ SyntaxError "Unclosed string literal"

genTokenSeq False acc token (curChar:nextInput)
    | curChar `elem` "()"    = keepDelim acc token curChar nextInput
    | curChar `elem` " \n\t" = dropDelim acc token curChar nextInput
    | curChar == '"'         = startString acc token curChar nextInput
    | otherwise              = appendChar False acc token curChar nextInput

genTokenSeq True acc token (curChar:nextInput)
    | curChar == '"' = endString acc token curChar nextInput
    | otherwise      = appendChar True acc token curChar nextInput
