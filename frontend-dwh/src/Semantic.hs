{-# LANGUAGE OverloadedStrings #-}

module Semantic 
    ( checkProgram
    , SemanticError
    ) where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type SemanticError = Text
type Env = Map Text Type 
type SemanticCtx a = StateT Env (Either SemanticError) a

checkProgram :: Program -> Either SemanticError [TypedStmt]
checkProgram prog = evalStateT (mapM checkStmt prog) Map.empty

checkStmt :: Stmt -> SemanticCtx TypedStmt
checkStmt (SVar varName expr) = do
    -- check before expr
    -- [ERROR]: var x = x + 1
    tExpr@(TypedExpr typ _) <- checkExpr expr
    modify (Map.insert varName typ)
    return $ TSVar varName tExpr

checkStmt (SExpr expr) = do
    tExpr <- checkExpr expr 
    return $ TSExpr tExpr

checkExpr :: Expr -> SemanticCtx TypedExpr
checkExpr expr@(EVar varName) = do
    env <- get
    case Map.lookup varName env of
        Just typ -> return $ TypedExpr typ expr
        Nothing -> lift $ Left $ "The variable '" <> varName <> "' has not been declared"

checkExpr expr@(ELit _) = return $ TypedExpr TDouble expr
checkExpr expr@(EString _) = return $ TypedExpr TString expr
checkExpr expr@(EBool _) = return $ TypedExpr TBool expr

checkExpr expr@(EAdd e1 e2) = checkBinOp expr e1 e2
checkExpr expr@(ESub e1 e2) = checkBinOp expr e1 e2
checkExpr expr@(EMult e1 e2) = checkBinOp expr e1 e2
checkExpr expr@(EDiv e1 e2) = checkBinOp expr e1 e2
checkExpr expr@(EPow e1 e2) = checkBinOp expr e1 e2

checkExpr expr@(ESqrt e) = do
    TypedExpr t _ <- checkExpr e
    case (t) of
        TDouble -> return $ TypedExpr TDouble expr
        _        -> lift $ Left "Types Error: expect Double"

checkBinOp :: Expr -> Expr -> Expr -> SemanticCtx TypedExpr
checkBinOp parentExpr e1 e2 = do
    TypedExpr t1 _ <- checkExpr e1
    TypedExpr t2 _ <- checkExpr e2
    case (t1, t2) of
        (TDouble, TDouble) -> return $ TypedExpr TDouble parentExpr
        _                  -> lift $ Left "Types Error: invalid combination use Double and Double"
