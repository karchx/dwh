{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax
    ( Expr(..)
    , Stmt(..)
    , Type(..)
    , Program
    , Parser
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data Type = TDouble | TBool | TString deriving (Show, Eq)

data Expr a
    = EVar a Text
    | ELit a Double
    | EString a Text
    | EBool a Bool
    | EAdd a (Expr a) (Expr a)
    | ESub a (Expr a) (Expr a)
    | EMult a (Expr a) (Expr a)
    | EDiv a (Expr a) (Expr a)
    | EPow a (Expr a) (Expr a)
    | ESqrt a (Expr a)
    deriving (Show, Eq, Functor)

data Stmt a
    = SVar a Text (Expr a)
    | SExpr a (Expr a)
    deriving (Show, Eq, Functor)

type Program a = [Stmt a]
