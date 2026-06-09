{-# LANGUAGE OverloadedStrings #-}
module Lexer 
    ( sc
    , lexeme
    , symbol
    , reserveWords
    , parens
    , rword
    , double
    , stringLit
    , identifier
    , braces
    , commaSep
    ) where

import Syntax (Parser)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "**") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserveWords :: [Text]
reserveWords = ["var", "true", "false", "sqrt", "fun", "return"]

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol "," 

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

double :: Parser Double
double = lexeme (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

stringLit :: Parser Text
stringLit = lexeme $ do
    _ <- char '"'
    str <- manyTill L.charLiteral (char '"')
    return (T.pack str)

identifier :: Parser Text
identifier = lexeme . try $ do
    ident <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    if ident `elem` reserveWords
        then empty
        else return ident

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar) 
