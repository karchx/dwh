-- eval is intended solely for internal use by the AST debugger.
{-# LANGUAGE OverloadedStrings #-}

module Eval (eval) where

import Syntax (Expr(..))
import Data.Text (Text)
import qualified Data.Text as T

type Env = [(Text, Double)]

eval :: Env -> Expr -> Double
eval _ (ELit n) = n
eval env (EAdd e1 e2) = eval env e1 + eval env e2
eval env (ESub e1 e2) = eval env e1 - eval env e2
eval env (EMult e1 e2) = eval env e1 * eval env e2
eval env (EDiv e1 e2) = eval env e1 / eval env e2
eval env (EPow e1 e2) = eval env e1 ** eval env e2
eval env (ESqrt e1) = sqrt (eval env e1)
eval env (EVar x ) = case lookup x env of
    Just val -> val
    Nothing -> error $ "Unbound variable " ++ T.unpack x
eval _ _ = error "Not implemented yet"