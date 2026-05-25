{-# LANGUAGE OverloadedStrings #-}

module Semantic 
    ( checkProgram
    , SemanticError
    ) where

import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (unless)
import Control.Monad.State

type SemanticError = Text
type Env = Set Text
type SemanticCtx a = StateT Env (Either SemanticError) a

checkProgram :: Program -> Either SemanticError ()
checkProgram prog = evalStateT (checkStmts prog) Set.empty

checkStmts :: [Stmt] -> SemanticCtx ()
checkStmts = mapM_ checkStmt

checkStmt :: Stmt -> SemanticCtx ()
checkStmt (SVar varName expr) = do
    -- check before expr
    -- [ERROR]: var x = x + 1
    checkExpr expr
    modify (Set.insert varName)
checkStmt (SExpr expr) = do
    checkExpr expr 

checkExpr :: Expr -> SemanticCtx ()
checkExpr (EVar varName) = do
    env <- get
    unless (Set.member varName env) $
        lift $ Left $ "The variable '" <> varName <> "' has not been declared"

checkExpr (ELit _) = return ()
checkExpr (EBool _) = return ()

checkExpr (EAdd e1 e2) = checkBinOp e1 e2
checkExpr (ESub e1 e2) = checkBinOp e1 e2
checkExpr (EMult e1 e2) = checkBinOp e1 e2
checkExpr (EDiv e1 e2) = checkBinOp e1 e2
checkExpr (EPow e1 e2) = checkBinOp e1 e2

checkExpr (ESqrt e) = checkExpr e

checkBinOp :: Expr -> Expr -> SemanticCtx ()
checkBinOp e1 e2 = checkExpr e1 >> checkExpr e2
