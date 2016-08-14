-- |Implementations of semantic rules for evaluating expressions in hasp.

module Semantics
( evalExpr
) where

import qualified Data.Map as Map

import Error
import Expressions
import DataTypes

evalArgExprs_recurse :: [HData] -> Env -> [Expr] -> Either Error [HData]
evalArgExprs_recurse acc env [] = Right acc
evalArgExprs_recurse acc env (expr:exprs) =
    case evalExpr env expr of
        Left err     -> Left err
        Right (result, _) -> evalArgExprs_recurse (acc ++ [result]) env exprs
        -- Ignoring any new environment, since hasp expressions cannot affect
        -- name bindings at a higher-level scope, apart from define, which is
        -- valid only as a top-level expression.

evalArgExprs :: Env -> [Expr] -> Either Error [HData]
evalArgExprs = evalArgExprs_recurse []

evalFuncCall :: Env -> Expr -> Either Error (HData, Env)
evalFuncCall env (List (headExpr:argExprs)) =  -- TODO: Actually use closures.
    case evalExpr env headExpr of
        Right (HFunc closure f, _) ->
            case evalArgExprs env argExprs of
                Left err   -> Left err
                Right args ->
                    case f args of
                        Left err     -> Left err
                        Right result -> Right (result, env)
        Left err -> Left err
        Right (result, _) ->
            Left . Error $ "Cannot evaluate `" ++ (show result) ++ "`"

evalExpr :: Env -> Expr -> Either Error (HData, Env)

evalExpr env (Atom (StringLiteral str)) = Right (HString str, env)
evalExpr env (Atom (IntLiteral int)) = Right (HN $ HInt int, env)
evalExpr env (Atom (FloatLiteral float)) = Right (HN $ HFloat float, env)
evalExpr env (Atom (BoolLiteral bool)) = Right (HBool bool, env)

evalExpr (Env envMap) (Atom (Id ident)) =
    case Map.lookup ident envMap of
        Just value -> Right (value, Env envMap)
        Nothing    -> Left . NameError $ "Unbound identifier `" ++ ident ++ "`"

-- TODO: See if it's possible to neatly propagate errors without deeply nested
-- case statements.
evalExpr env (List []) = Right (HList [], env)

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
data SyntacticKeyword = SK Bool (Env -> [Expr] -> Either Error (HData, Env))

type SKMap = Map.Map Identifier SyntacticKeyword
defaultSK :: SKMap
defaultSK = Map.fromList
    [ ("define", SK True define)
    , ("lambda", undefined)
    , ("if", undefined)
    , ("and", undefined)
    , ("or", undefined) ]

-- |(define <name> <expr>)
define :: Env -> [Expr] -> Either Error (HData, Env)
define _ [] = Left $ errNumArgs 2 0
define _ (_:[]) = Left $ errNumArgs 2 1

-- TODO: define shouldn't really return anything.
define (Env envMap) [Atom (Id name), expr] =
    case evalExpr (Env envMap) expr of
        Left err -> Left err
        Right (result, _) ->
            Right (HList [], Env $ Map.insert name result envMap)

define _ [_, _] = Left errWrongType
define _ args = Left $ errNumArgs 2 $ length args
