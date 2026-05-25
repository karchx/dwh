{-# LANGUAGE OverloadedStrings #-}

module Syntax
    ( Expr(..)
    , Stmt(..)
    , Program
    , Parser
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data Expr
    = EVar Text
    | ELit Double
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


type Program = [Stmt]