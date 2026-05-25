{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import Syntax 
    ( Parser
    , Expr(..)
    , Stmt(..)
    , Program)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import qualified Data.Text as T

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "**") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

double :: Parser Double
double = lexeme (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

reserveWords :: [Text]
reserveWords = ["var"]

identifier :: Parser Text
identifier = lexeme (p >>= check . T.pack)
    where
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reserveWords
                  then fail $ "The keyword '" ++ T.unpack x ++ "' is identifier"
                  else return x

stmt :: Parser Stmt
stmt = try varDecl <|> SExpr <$> expr

expr :: Parser Expr
expr = makeExprParser term operatorTable

term :: Parser Expr
term = try factor

factor :: Parser Expr
factor = choice
    [ parens expr
    , ELit <$> double
    , EVar <$> identifier
    ]

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar) 

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
    , [ Prefix (ESqrt <$ symbol "sqrt") ]

    , [ InfixL (EMult <$ symbol "*")
      , InfixL (EDiv <$ symbol "/") ]

    , [ InfixL (EAdd <$ symbol "+")
      , InfixL (ESub <$ symbol "-") ]
    ]
