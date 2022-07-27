module Helpers where

import Ast
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseSpace :: Parser ()
parseSpace =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{--" "--}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme parseSpace

symbol :: String -> Parser String
symbol sb = try $ L.symbol parseSpace sb

lowerKeywords :: [String]
lowerKeywords = ["fn", "let", "rec", "in", "if", "then", "else", "true", "false", "match", "with"]

reservedOperators :: [String]
reservedOperators = ["->"]

comments :: Parser ()
comments = parseSpace

many1 :: Parser a -> Parser [a]
many1 parser = (:) <$> parser <*> many parser

digit :: Parser (Token [Char])
digit = oneOf "0123456789"

whitespace :: Parser ()
whitespace = parseSpace >> comments

tok :: Parser a -> Parser a
tok = (<* whitespace)

parseIdent :: Parser Var
parseIdent = (lexeme . try) $ do
  a <- letterChar
  b <- many $ alphaNumChar <|> char '_' <|> char '\''
  check (a : b)
  where
    check x =
      if x `elem` lowerKeywords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else return x
