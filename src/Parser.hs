-- |Parser for converting tokenized hasp source code to abstract syntax trees.

module Parser
( parseExprs
, parseAtom
, Token
) where

import Data.Sequence
import Data.Foldable (toList)
import Control.Monad (liftM)
import Text.Read
import Text.Regex.Posix hiding (empty)

import Expressions
import Error
import Tokenizer

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x stack = x:stack

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (top:rest) = (Just top, rest)

squashTwo :: (a -> a -> a) -> Stack a -> Stack a
squashTwo f [] = []
squashTwo f (first:rest) =
    let (maybeSecond, remain) = pop rest
    in  case maybeSecond of
        Nothing -> first:rest
        Just second -> push (first `f` second) remain

appendTo :: Seq Expr -> Seq Expr -> Seq Expr
x `appendTo` lst = lst |> (List $ toList x)

integerPat = "^-?[0-9]+$"
floatPat = "^-?[0-9]+\\.[0-9]+$" -- TODO: Parse scientific notation.
boolPat = "^(#t|#f)$"

stringPat = "^\"([^\"]|\\\")*\"$"

varNamePat = "^([a-zA-Z]|-|[!@$%&*_=+|<>/?])([a-zA-Z0-9]|-|[!@$%&*_=+|<>/?])*$"

-- |Parses a (supposedly) atomic expression to determine its type, and checks
-- that it is indeed a legal atom.
parseAtom :: Token -> ThrowsError Expr
parseAtom token
    | (token =~ integerPat :: Bool) =
        let maybeVal = readMaybe token :: Maybe Integer
        in  case maybeVal of
                Nothing    -> throw parseFailure
                Just value -> return . Atom $ IntLiteral value
    | (token =~ floatPat :: Bool)   =
        let maybeVal = readMaybe token :: Maybe Float
        in  case maybeVal of
            Nothing    -> throw parseFailure
            Just value -> return . Atom $ FloatLiteral value
    | (token =~ boolPat :: Bool)    =
        case token of
            "#t" -> return . Atom $ BoolLiteral True
            "#f" -> return . Atom $ BoolLiteral False
            _    -> throw parseFailure
    | (token =~ stringPat :: Bool)  = return . Atom $ StringLiteral token
    | (token =~ varNamePat :: Bool) = return . Atom $ Id token
    | otherwise                     = throw parseFailure
    where
        parseFailure = SyntaxError $ "Invalid atomic symbol `" ++ token ++ "`"

-- |Parses hasp source code, represented as a string, producing a list of hasp
-- expressions, in order from left to right, represnted by their abstract
-- syntax trees.
--
-- The algorithm used by this parser is essentially a simple pushdown automaton.
-- It traverses list of tokens from left to right, maintaining a stack to
-- indicate the nesting level within the current top-level expression, as well
-- as a list of top-level expressions that have been parsed so far. At each
-- token, the algorithm does the following, depending on the current token.
--
--     "(":   Push a new empty list onto the stack.
--
--     ")":   Squash top two elements of the stack together, by taking the top
--            and appending it to the list immediately below it.
--
--     other: An atomic value - append it to the list on top of the stack. If
--            the stack is empty then we have a loose atom, so just return it.
traverseExprTokens :: Stack (Seq Expr) -> Seq Expr -> [Token] ->
    ThrowsError (Seq Expr)
traverseExprTokens [] acc [] = return acc
traverseExprTokens (top:rest) _ [] = throw $ SyntaxError "Unclosed parenthesis"

traverseExprTokens stack acc ("(":tokens) =
    traverseExprTokens (push empty stack) acc tokens

traverseExprTokens [] acc ( ")":tokens) =
    throw $ SyntaxError "Unexpected token \")\""
traverseExprTokens (top:[]) acc ( ")":tokens) =
    traverseExprTokens [] (acc |> (List $ toList top)) tokens
traverseExprTokens (top:rest) acc ( ")":tokens) =
    traverseExprTokens (squashTwo appendTo (top:rest)) acc tokens

traverseExprTokens [] acc (atom:tokens) = do
    val <- parseAtom atom
    traverseExprTokens [] (acc |> val) tokens
traverseExprTokens (top:rest) acc (atom:tokens) = do
    val <- parseAtom atom
    traverseExprTokens (push (top |> val) rest) acc tokens

parseExprs :: [Token] -> ThrowsError [Expr]
parseExprs tokens = liftM toList $ traverseExprTokens [] empty tokens

