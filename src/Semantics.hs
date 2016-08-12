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
        Right result -> evalArgExprs_recurse (acc ++ [result]) env exprs

evalArgExprs :: Env -> [Expr] -> Either Error [HData]
evalArgExprs = evalArgExprs_recurse []

evalExpr :: Env -> Expr -> Either Error HData

evalExpr env (Atom (StringLiteral str)) = Right (HString str)
evalExpr env (Atom (IntLiteral int)) = Right (HInt int)
evalExpr env (Atom (FloatLiteral float)) = Right (HFloat float)
evalExpr env (Atom (BoolLiteral bool)) = Right (HBool bool)

evalExpr (Env envMap) (Atom (Var varName)) =
    case Map.lookup varName envMap of
        Just value -> Right value
        Nothing    -> undefined -- Error, undefined symbol

-- TODO: See if it's possible to neatly propagate errors without deeply nested case statements.
evalExpr env (List []) = Right (HList [])
evalExpr env (List (functionExpr:argExprs)) =  -- TODO: Actually use closures.
    case evalExpr env functionExpr of
        Right (HFunc closure nargs f) ->
            case evalArgExprs env argExprs of
                Left err   -> Left err
                Right args ->
                    case f args of
                        Left err     -> Left err
                        Right result -> Right result
        _ -> undefined -- Error - not a function
