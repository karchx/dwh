{-# LANGUAGE OverloadedStrings #-}

module Parser (program) where

import Syntax
import Lexer
import Data.Text (Text)
import Text.Megaparsec
import Control.Monad.Combinators.Expr

stmt :: Parser (Stmt SourcePos)
stmt = varDecl <|> (SExpr <$> getSourcePos <*> expr)

expr :: Parser (Expr SourcePos)
expr = makeExprParser term operatorTable

term :: Parser (Expr SourcePos)
term = factor

pBool :: Parser (Expr SourcePos)
pBool = do
  pos <- getSourcePos
  (EBool pos True <$ rword "true") <|> (EBool pos False <$ rword "false")

pString :: Parser (Expr SourcePos)
pString = EString <$> getSourcePos <*> stringLit

factor :: Parser (Expr SourcePos)
factor = choice
    [ parens expr
    , pBool
    , pString
    , ELit <$> getSourcePos <*> double
    , EVar <$> getSourcePos <*> identifier
    ]

varDecl :: Parser (Stmt SourcePos)
varDecl = do
    pos <- getSourcePos
    _ <- rword "var"
    x <- identifier
    _ <- symbol "="
    e <- expr
    return $ SVar pos x e

program :: Parser (Program SourcePos)
program = between sc eof (many stmt)

binOp :: (SourcePos -> Expr SourcePos -> Expr SourcePos -> Expr SourcePos) 
      -> Text 
      -> Parser (Expr SourcePos -> Expr SourcePos -> Expr SourcePos)
binOp constructor sym = do
  pos <- getSourcePos
  _ <- symbol sym
  return (constructor pos)

preOp :: (SourcePos -> Expr SourcePos -> Expr SourcePos) -> Text -> Parser (Expr SourcePos -> Expr SourcePos)
preOp constructor sym = do
  pos <- getSourcePos
  _ <- rword sym
  return (constructor pos)

operatorTable :: [[Operator Parser (Expr SourcePos)]]
operatorTable =
    [ [ InfixR (binOp EPow "^") ]
    , [ Prefix (preOp ESqrt "sqrt") ]

    , [ InfixL (binOp EMult "*")
      , InfixL (binOp EDiv "/") ]

    , [ InfixL (binOp EAdd "+")
      , InfixL (binOp ESub "-") ]
    ]
