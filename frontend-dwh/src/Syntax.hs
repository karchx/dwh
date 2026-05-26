{-# LANGUAGE OverloadedStrings #-}

module Syntax
    ( Expr(..)
    , Stmt(..)
    , Type(..)
    , TypedStmt(..)
    , TypedExpr(..)
    , Program
    , Parser
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data Type = TDouble | TBool | TString deriving (Show, Eq)

data Expr
    = EVar Text
    | ELit Double
    | EString Text
    | EBool Bool
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMult Expr Expr
    | EDiv Expr Expr
    | EPow Expr Expr
    | ESqrt Expr
    deriving (Show, Eq)

data Stmt
    = SVar Text Expr
    | SExpr Expr
    deriving (Show, Eq)

data TypedExpr = TypedExpr Type Expr deriving (Show, Eq)

data TypedStmt
    = TSVar Text TypedExpr
    | TSExpr TypedExpr
    deriving (Show, Eq)

type Program = [Stmt]
