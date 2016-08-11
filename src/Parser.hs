module Parser where

import Expressions

-- TODO: Have a better error handling mechanism.
data SyntaxError = SyntaxError String

type Token = String

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x stack = x:stack

pop :: Stack a -> (Maybe a, Stack a)
pop [] = (Nothing, [])
pop (top:rest) = (Just top, rest)

peek :: Stack a -> Maybe a
peek [] = Nothing
peek (top:rest) = Just top

squashTwo :: (a -> a -> a) -> Stack a -> Stack a
squashTwo f [] = []
squashTwo f (first:rest) =
    let (maybeSecond, remain) = pop rest
    in  case maybeSecond of
        Nothing -> first:rest
        Just second -> push (first `f` second) remain

appendTo :: Expr -> Expr -> Expr
x `appendTo` (List lst) = List (lst ++ [x])


-- Just testing this out. TODO: Make this pure (and actually work).
-- From some basic preliminary testing, this seems to work on a few valid examples that I've tried, however it fails to
-- properly recognize certain syntax errors.

-- Traverses list of tokens from left to right, maintaining a stack to indicate the current nesting level. At each
-- token, the algorithm does the following, depending on the current token.
--     "(":   Push a new empty list onto the stack.
--     ")":   Squash top two elements of the stack together, by taking the top and appending it to the list immediately
--            below it.
--     other: An atomic value - append it to the list on top of the stack. If the stack is empty then we have a loose
--            atom, so just return it.
traverseExprTokens :: Stack Expr -> [Token] -> Expr
traverseExprTokens [] [] = undefined -- Empty expression
traverseExprTokens (top:rest) [] =
    if null rest
        then top
        else undefined -- Syntax error: Missing )

traverseExprTokens stack ("(":tokens) = 
    traverseExprTokens (push (List []) stack) tokens

traverseExprTokens [] (")":tokens) = undefined -- Syntax error: Extra )
traverseExprTokens (top:rest) (")":tokens) =
    traverseExprTokens (squashTwo (appendTo) (top:rest)) tokens

traverseExprTokens [] (atom:_) = Atom (Var atom) -- Loose atom. Do anything with the remaining tokens?
traverseExprTokens (top:rest) (atom:tokens) =
    traverseExprTokens (push ((Atom (Var atom)) `appendTo` top) rest) tokens

parseExpr :: [Token] -> Expr
parseExpr = traverseExprTokens []

