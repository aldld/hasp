-- |Implementations of semantic rules for evaluating expressions in hasp.

module Semantics
( evalExpr
) where

import qualified Data.Map as Map

import Error
import Expressions
import DataTypes

evalArgExprs_recurse :: [HData] -> Env -> [Expr] -> ThrowsError [HData]
evalArgExprs_recurse acc env [] = return acc
evalArgExprs_recurse acc env (expr:exprs) = do
    (result, _) <- evalExpr env expr
    evalArgExprs_recurse (acc ++ [result]) env exprs
    -- Ignoring any new environment, since hasp expressions cannot affect
    -- name bindings at a higher-level scope, apart from define, which is
    -- valid only as a top-level expression.

evalArgExprs :: Env -> [Expr] -> ThrowsError [HData]
evalArgExprs = evalArgExprs_recurse []

evalFuncCall :: Env -> Expr -> ThrowsError (HData, Env)
evalFuncCall env (List (headExpr:argExprs)) = do
    (result, _) <- evalExpr env headExpr
    case result of
        HFunc closure f -> do
            args <- evalArgExprs env argExprs
            result <- f (Env $ Map.union (toMap env) (toMap closure)) args
            return (result, env)
        result -> throw . Error $ "Cannot evaluate `" ++ (show result) ++ "`"

evalExpr :: Env -> Expr -> ThrowsError (HData, Env)

evalExpr env (Atom (StringLiteral str)) = return (HString str, env)
evalExpr env (Atom (IntLiteral int)) = return (HN $ HInt int, env)
evalExpr env (Atom (FloatLiteral float)) = return (HN $ HFloat float, env)
evalExpr env (Atom (BoolLiteral bool)) = return (HBool bool, env)

evalExpr (Env envMap) (Atom (Id ident)) =
    case Map.lookup ident envMap of
        Just value -> return (value, Env envMap)
        Nothing    -> throw . NameError $ "Unbound identifier `" ++ ident ++ "`"

evalExpr env (List []) = return (HList [], env)

evalExpr env (List ((Atom (Id ident)):argExprs)) =
    case Map.lookup ident defaultSK of
        -- Syntactic keyword use.
        Just (SK topLevelOnly op) -> op env argExprs
        -- Function call.
        Nothing -> evalFuncCall env (List ((Atom (Id ident)):argExprs))

evalExpr env (List (headExpr:argExprs)) =
    evalFuncCall env (List (headExpr:argExprs))


-- Syntactic keywords in hasp that cannot strictly be functions, involving
-- features such as mutation and short circuiting that cannot be directly
-- implemented as functions.
--
-- Within hasp code, these expressions have the form
--
--     (<syntactic-keyword> <expr> ...)
--
-- and operate directly on its local portion of the abstract syntax tree and
-- compute results and/or modify the execution environment.

-- SK <top-level-only> <operation>
data SyntacticKeyword = SK Bool (Env -> [Expr] -> ThrowsError (HData, Env))

type SKMap = Map.Map Identifier SyntacticKeyword
defaultSK :: SKMap
defaultSK = Map.fromList
    [ ("define", SK True define)
    , ("lambda", SK False lambda)
    , ("if", SK False ifStmt)
    , ("and", undefined)
    , ("or", undefined) ]


-- |define keyword used for creating global variable name bindings. This
-- operation may overwrite existing bindings, if present.
--
-- Syntax: (define <name> <expr>)
define :: Env -> [Expr] -> ThrowsError (HData, Env)
define _ [] = throw $ errNumArgs 2 0
define _ (_:[]) = throw $ errNumArgs 2 1

-- TODO: define shouldn't really return anything.
define (Env envMap) [Atom (Id name), expr] = do
    (result, _) <- evalExpr (Env envMap) expr
    return (HList [], Env $ Map.insert name result envMap)

define _ [_, _] = throw errWrongType
define _ args = throw . errNumArgs 2 $ length args


-- |lambda keyword for creating anonymous functions within hasp code. This
-- includes an (admittedly rudimentary) implementation of closures, in the sense
-- that the environment in which an anonymous function is created is stored
-- in the function returned by lambda.
--
-- Syntax: (lambda (<arg-ids> ...) <expr>)
lambda :: Env -> [Expr] -> ThrowsError (HData, Env)
lambda _ [] = throw $ errNumArgs 2 0
lambda _ (_:[]) = throw $ errNumArgs 2 1

lambda env [(List argExprs), expr] = do
    argNames <- getArgNames [] argExprs
    return (HFunc env $ makeLambda env argNames expr, env)


lambda _ [_, _] = throw errWrongType
lambda _ args = throw . errNumArgs 2 $ length args

getArgNames :: [Identifier] -> [Expr] -> ThrowsError [Identifier]
getArgNames acc [] = return acc
getArgNames acc ((Atom (Id ident)):exprs) = getArgNames (acc ++ [ident]) exprs
getArgNames _ (expr:_) =
    throw . Error $ "Invalid argument identifier `" ++ (show expr) ++
        "` in lambda expression"

makeLambda :: Env -> [Identifier] -> Expr ->
    (Env -> [HData] -> ThrowsError HData)
makeLambda (Env envMap) argNames expr callerEnv args =
    if expectedNumArgs == actualNumArgs
        then do
            let internEnv = Env $ foldr1 Map.union
                    [Map.fromList $ zip argNames args, toMap callerEnv, envMap]
            (result, _) <- evalExpr internEnv expr
            return result
    else throw $ errNumArgs expectedNumArgs actualNumArgs
    where
        env = Env envMap
        expectedNumArgs = length argNames
        actualNumArgs = length args

-- |if statement that evaluates the first argument. If the condition evaluates
-- to true, then the then-expression is evaluated and its result returned,
-- otherwise the else-expression is evaluated and its result is returned.
--
-- Syntax: (if <cond> <then-expr> <else-expr>)
ifStmt :: Env -> [Expr] -> ThrowsError (HData, Env)
ifStmt _ [] = throw $ errNumArgs 3 0
ifStmt _ (_:[]) = throw $ errNumArgs 3 1
ifStmt _ (_:_:[]) = throw $ errNumArgs 3 2

ifStmt env [condExpr, thenExpr, elseExpr] = do
    (condResult, _) <- evalExpr env condExpr
    case condResult of
        HBool cond ->
            if cond
                then evalExpr env thenExpr
                else evalExpr env elseExpr
        val -> throw . TypeError $
            "Condition for if statment must be boolean, not `" ++
                (show val) ++ "`"

ifStmt _ args = throw . errNumArgs 3 $ length args
