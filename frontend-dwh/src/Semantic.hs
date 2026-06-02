{-# LANGUAGE OverloadedStrings #-}

module Semantic 
    ( checkProgram
    , SemanticError(..)
    ) where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Text.Megaparsec.Pos (SourcePos)

data SemanticError = ErrorAt SourcePos Text deriving (Show, Eq)
type Env = Map Text Type 
type SemanticCtx a = StateT Env (Either SemanticError) a

checkProgram :: Program SourcePos -> Either SemanticError (Program (SourcePos, Type))
checkProgram prog = evalStateT (mapM checkStmt prog) Map.empty

checkStmt :: Stmt SourcePos -> SemanticCtx (Stmt (SourcePos, Type))
checkStmt (SVar pos varName expr) = do
    -- check before expr
    -- [ERROR]: var x = x + 1
    tExpr <- checkExpr expr
    modify (Map.insert varName (getType tExpr))
    return $ SVar (pos, getType tExpr) varName tExpr

checkStmt (SExpr pos expr) = do
    tExpr <- checkExpr expr 
    return $ SExpr (pos, getType tExpr) tExpr

checkExpr :: Expr SourcePos -> SemanticCtx (Expr (SourcePos, Type))
checkExpr (EVar pos varName) = do
    env <- get
    case Map.lookup varName env of
        Just typ -> return $ EVar (pos, typ) varName
        Nothing -> lift $ Left $ ErrorAt pos ("The variable '" <> varName <> "' has not been declared")

checkExpr (ELit pos val) = return $ ELit (pos, TDouble) val
checkExpr (EString pos val) = return $ EString (pos, TString) val
checkExpr (EBool pos val) = return $ EBool (pos, TBool) val

checkExpr (EAdd  pos e1 e2) = checkBinOp pos EAdd e1 e2
checkExpr (ESub  pos e1 e2) = checkBinOp pos ESub e1 e2
checkExpr (EMult pos e1 e2) = checkBinOp pos EMult e1 e2
checkExpr (EDiv  pos e1 e2) = checkBinOp pos EDiv e1 e2
checkExpr (EPow  pos e1 e2) = checkBinOp pos EPow e1 e2

checkExpr (ESqrt pos e) = do
    tExpr <- checkExpr e
    case getType tExpr of
        TDouble -> return $ ESqrt (pos, TDouble) tExpr
        _        -> lift $ Left $ ErrorAt pos "Types Error: expect Double"

checkBinOp :: SourcePos 
           -> ((SourcePos, Type) -> Expr (SourcePos, Type) -> Expr (SourcePos, Type) -> Expr (SourcePos, Type)) 
           -> Expr SourcePos 
           -> Expr SourcePos 
           -> SemanticCtx (Expr (SourcePos, Type))
checkBinOp pos constructor e1 e2 = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case (getType t1, getType t2) of
        (TDouble, TDouble) -> return $ constructor (pos, TDouble) t1 t2
        _                  -> lift $ Left $ ErrorAt pos "Types Error: invalid combination use Double and Double"


getType :: Expr (SourcePos, Type) -> Type
getType (EVar (_, t) _) = t
getType (ELit (_, t) _) = t
getType (EString (_, t) _) = t
getType (EBool (_, t) _) = t
getType (EAdd (_, t) _ _) = t
getType (ESub (_, t) _ _) = t
getType (EMult (_, t) _ _) = t
getType (EDiv (_, t) _ _) = t
getType (EPow (_, t) _ _) = t
getType (ESqrt (_, t) _) = t
