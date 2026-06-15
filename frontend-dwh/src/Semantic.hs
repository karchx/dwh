{-# LANGUAGE OverloadedStrings #-}

module Semantic 
    ( checkProgram
    , SemanticError(..)
    ) where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as Map
import Control.Monad.State
import Text.Megaparsec.Pos (SourcePos)

data SemanticError = ErrorAt SourcePos Text deriving (Show, Eq)
type Env = Map Text Type 
type SemanticCtx a = StateT Env (Either SemanticError) a

checkProgram :: Program SourcePos -> Either SemanticError (Program (SourcePos, Type))
checkProgram prog = evalStateT (mapM checkStmt prog) Map.empty

checkStmt :: Stmt SourcePos -> SemanticCtx (Stmt (SourcePos, Type))
checkStmt (SFun pos name params body) = do
    globalEnv <- get
    -- TODO: TDouble forced, replacement for Hindley-Milner.
    let localEnv = foldr (\p env -> Map.insert p TDouble env) globalEnv params
    put localEnv

    checkBody <- mapM checkStmt body
    let retType = extractReturnType checkBody

    put (Map.insert name (TFun (replicate (length params) TDouble) retType) globalEnv)

    return $ SFun (pos, TFun (replicate (length params) TDouble) retType) name params checkBody

checkStmt (SReturn pos expr) = do
    tExpr <- checkExpr expr
    return $ SReturn (pos, getType tExpr) tExpr

checkStmt (SAssign pos vars expr) = do
    tExpr <- checkExpr expr
    let exprType = getType tExpr
    case (vars, exprType) of
        -- Simple assign
        ([v], _) -> do
            modify (Map.insert v exprType)
            return $ SAssign (pos, exprType) vars tExpr

        -- Get identifier functions, e.g:
        -- connector, result = function()
        ([status, result], _) -> do
            case tExpr of
                EApp _ _ _ -> do
                    modify (Map.insert status TBool)
                    modify (Map.insert result exprType)
                    return $ SAssign (pos, exprType) vars tExpr
                _ -> lift $ Left $ ErrorAt pos "The assign failed"

        -- Malformed
        _ -> lift $ Left $ ErrorAt pos "Malformed assign"

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
checkExpr (EConnect pos e1 e2) = checkBinOp pos EConnect e1 e2
checkExpr (ESysCall pos name args) = do
    tArgs <- mapM checkExpr args
    return $ ESysCall (pos, TDouble) name tArgs
checkExpr (ESqrt pos e) = do
    tExpr <- checkExpr e
    case getType tExpr of
        TDouble -> return $ ESqrt (pos, TDouble) tExpr
        _        -> lift $ Left $ ErrorAt pos "Types Error: expect Double"

checkExpr (EApp pos fun args) = do
    tFun <- checkExpr fun
    tArgs <- mapM checkExpr args
    let argTypes = map getType tArgs
    case getType tFun of
        TFun paramTypes retType -> do
            if length argTypes /= length paramTypes
                then lift $ Left $ ErrorAt pos "Arity mismatch in the function call"
                else if argTypes /= paramTypes
                    then lift $ Left $ ErrorAt pos "Type mismatch in the function arguments"
                    else return $ EApp (pos, retType) tFun tArgs
        _ -> lift $ Left $ ErrorAt pos "Attempt to call an expression that is not a function"

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
        (TString, TString) -> return $ constructor (pos, TString) t1 t2
        _                  -> lift $ Left $ ErrorAt pos "Types Error: invalid combination use Double and Double"

extractReturnType :: [Stmt (SourcePos, Type)] -> Type
extractReturnType stmts = fromMaybe TVoid $ listToMaybe [getType expr | SReturn _ expr <- stmts]

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
getType (EApp (_, t) _ _) = t
getType (ESysCall (_, t) _ _) = t
getType (EConnect (_, t) _ _) = t
