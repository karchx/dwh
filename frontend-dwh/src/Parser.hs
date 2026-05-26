{-# LANGUAGE OverloadedStrings #-}

module Parser (program) where

import Syntax
import Lexer
import Text.Megaparsec
import Control.Monad.Combinators.Expr

stmt :: Parser Stmt
stmt = varDecl <|> (SExpr <$> expr)

expr :: Parser Expr
expr = makeExprParser term operatorTable

term :: Parser Expr
term = factor

pBool :: Parser Expr
pBool = (EBool True <$ rword "true") <|> (EBool False <$ rword "false")

factor :: Parser Expr
factor = choice
    [ parens expr
    , pBool
    , ELit <$> double
    , EVar <$> identifier
    ]

varDecl :: Parser Stmt
varDecl = do
    _ <- rword "var"
    x <- identifier
    _ <- symbol "="
    e <- expr
    return $ SVar x e

program :: Parser Program
program = between sc eof (many stmt)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixR (EPow <$ symbol "^" ) ]
    , [ Prefix (ESqrt <$ rword "sqrt") ]

    , [ InfixL (EMult <$ symbol "*")
      , InfixL (EDiv <$ symbol "/") ]

    , [ InfixL (EAdd <$ symbol "+")
      , InfixL (ESub <$ symbol "-") ]
    ]
