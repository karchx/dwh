{-# LANGUAGE OverloadedStrings #-}
module Lexer 
    ( sc
    , lexeme
    , symbol
    , reserveWords
    , parens
    , rword
    , double
    , identifier
    ) where

import Syntax 
    ( Parser
    , Program)

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
reserveWords = ["var", "true", "false", "sqrt"]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

double :: Parser Double
double = lexeme (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer)))

identifier :: Parser Text
identifier = lexeme (p >>= check . T.pack)
    where
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reserveWords
                  then fail $ "Cannot use reserved keywordThe keyword '" ++ T.unpack x ++ "' as an identifier"
                  else return x

rword :: Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar) 
