module Parser
( parseExprs
, parseAtom
, Token
) where

import Text.Read
import Text.Regex.Posix
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

appendTo :: [Expr] -> [Expr] -> [Expr]
x `appendTo` lst = lst ++ [List x]

integerPat = "^-?[0-9]+$"
floatPat = "^-?[0-9]+\\.[0-9]+$" -- TODO: Support scientific notation.
boolPat = "^(#t|#f)$"
stringPat = "^\"([^\"]|\\\")*\"$" -- FIXME: This doesn't actually work!!! e.g. matches strings containing " character.
varNamePat = "^([a-zA-Z]|-|[!@$%&*_=+|<>/?])([a-zA-Z0-9]|-|[!@$%&*_=+|<>/?])*$" -- There must be a better way.

-- |Parses a (supposedly) atomic expression to determine its type, and checks that it is indeed a legal atom.
-- TODO: Make this more robust.
parseAtom :: Token -> Either Error Expr
parseAtom token
    | (token =~ integerPat :: Bool) =
        let maybeVal = readMaybe token :: Maybe Int
        in  case maybeVal of
                Nothing    -> parseFailure
                Just value -> Right (Atom (IntLiteral value))
    | (token =~ floatPat :: Bool)   =
        let maybeVal = readMaybe token :: Maybe Float
        in  case maybeVal of
            Nothing    -> parseFailure
            Just value -> Right (Atom (FloatLiteral value))
    | (token =~ boolPat :: Bool)    =
        case token of
            "#t" -> Right (Atom (BoolLiteral True))
            "#f" -> Right (Atom (BoolLiteral False))
            _    -> parseFailure
    | (token =~ stringPat :: Bool)  = Right (Atom (StringLiteral token))
    | (token =~ varNamePat :: Bool) = Right (Atom (Var token))
    | otherwise                     = parseFailure
    where
        parseFailure = Left (SyntaxError ("Invalid atomic symbol: " ++ token))


-- From some basic preliminary testing, this seems to work on a few valid examples that I've tried, however it fails to
-- properly recognize certain syntax errors.

-- Traverses list of tokens from left to right, maintaining a stack to indicate the current nesting level in the current
-- top-level expression, as well as a list of top-level expressions that have been parsed so far. At each token, the
-- algorithm does the following, depending on the current token.
--     "(":   Push a new empty list onto the stack.
--     ")":   Squash top two elements of the stack together, by taking the top and appending it to the list immediately
--            below it.
--     other: An atomic value - append it to the list on top of the stack. If the stack is empty then we have a loose
--            atom, so just return it.
traverseExprTokens :: Stack [Expr] -> [Expr] -> [Token] -> Either Error [Expr]
traverseExprTokens [] acc [] = Right acc
traverseExprTokens (top:rest) _ [] = Left (SyntaxError "Missing )")

traverseExprTokens stack acc ("(":tokens) = 
    traverseExprTokens (push [] stack) acc tokens

traverseExprTokens [] acc (")":tokens) = Left (SyntaxError "Extra )")
traverseExprTokens (top:[]) acc (")":tokens) =
    traverseExprTokens [] (acc ++ [List top]) tokens
traverseExprTokens (top:rest) acc (")":tokens) =
    traverseExprTokens (squashTwo appendTo (top:rest)) acc tokens

traverseExprTokens [] acc (atom:tokens) =
    let maybeVal = parseAtom atom
    in  case maybeVal of
            Left err  -> Left err
            Right val -> traverseExprTokens [] (acc ++ [val]) tokens
traverseExprTokens (top:rest) acc (atom:tokens) =
    let maybeVal = parseAtom atom
    in  case maybeVal of
            Left err  -> Left err
            Right val -> traverseExprTokens (push (top ++ [val]) rest) acc tokens

parseExprs :: [Token] -> Either Error [Expr]
parseExprs = traverseExprTokens [] []

