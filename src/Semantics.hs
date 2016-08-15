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
                    case f (Env $ Map.union (toMap env) (toMap closure)) args of
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
    , ("lambda", SK False lambda)
    , ("if", SK False ifStmt)
    , ("and", undefined)
    , ("or", undefined) ]


-- |define keyword used for creating global variable name bindings. This
-- operation may overwrite existing bindings, if present.
--
-- Syntax: (define <name> <expr>)
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
define _ args = Left . errNumArgs 2 $ length args


-- |lambda keyword for creating anonymous functions within hasp code. This
-- includes an (admittedly rudimentary) implementation of closures, in the sense
-- that the environment in which an anonymous function is created is stored
-- in the function returned by lambda.
--
-- Syntax: (lambda (<arg-ids> ...) <expr>)
lambda :: Env -> [Expr] -> Either Error (HData, Env)
lambda _ [] = Left $ errNumArgs 2 0
lambda _ (_:[]) = Left $ errNumArgs 2 1

lambda env [(List argExprs), expr] =
    case getArgNames [] argExprs of
        Left err -> Left err
        Right argNames -> Right (HFunc env $ makeLambda env argNames expr, env)

lambda _ [_, _] = Left errWrongType
lambda _ args = Left . errNumArgs 2 $ length args

getArgNames :: [Identifier] -> [Expr] -> Either Error [Identifier]
getArgNames acc [] = Right acc
getArgNames acc ((Atom (Id ident)):exprs) = getArgNames (acc ++ [ident]) exprs
getArgNames _ (expr:_) =
    Left . Error $ "Invalid argument identifier `" ++ (show expr) ++
        "` in lambda expression"

makeLambda :: Env -> [Identifier] -> Expr ->
    (Env -> [HData] -> Either Error HData)
makeLambda (Env envMap) argNames expr callerEnv args =
    if expectedNumArgs == actualNumArgs
        then
            let internEnv = Env $ foldr1 Map.union
                    [Map.fromList $ zip argNames args, toMap callerEnv, envMap]
            in  case evalExpr internEnv expr of
                Left err -> Left err
                Right (result, _) -> Right result
    else Left $ errNumArgs expectedNumArgs actualNumArgs
    where
        env = Env envMap
        expectedNumArgs = length argNames
        actualNumArgs = length args

-- |if statement that evaluates the first argument. If the condition evaluates
-- to true, then the then-expression is evaluated and its result returned,
-- otherwise the else-expression is evaluated and its result is returned.
--
-- Syntax: (if <cond> <then-expr> <else-expr>)
ifStmt :: Env -> [Expr] -> Either Error (HData, Env)
ifStmt _ [] = Left $ errNumArgs 3 0
ifStmt _ (_:[]) = Left $ errNumArgs 3 1
ifStmt _ (_:_:[]) = Left $ errNumArgs 3 2

ifStmt env [condExpr, thenExpr, elseExpr] =
    case evalExpr env condExpr of
        Left err -> Left err
        Right (HBool cond, _) ->
            if cond
            then evalExpr env thenExpr
            else evalExpr env elseExpr
        Right (val, _) -> Left . TypeError $
            "Condition for if statment must be boolean, not `" ++
                (show val) ++ "`"

ifStmt _ args = Left . errNumArgs 3 $ length args
