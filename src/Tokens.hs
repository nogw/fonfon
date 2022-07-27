module Tokens where

import Data.Char
import Helpers
import Text.Megaparsec
import Text.Megaparsec.Char

data Reserved
  = RKWFn
  | RKWLet
  | RKWIn
  | RKWArrow
  | RKWEqual
  | RKWRec
  | RKWIf
  | RKWThen
  | RKWElse
  | RKWFalse
  | RKWTrue
  | RKWWith
  | RKWMatch
  | RKWBar

charIgnoreCase :: Char -> Parser Char
charIgnoreCase c = char (toUpper c) <|> char (toLower c)

stringIgnoreCase :: String -> Parser String
stringIgnoreCase = foldr (\x -> (<*>) ((:) <$> charIgnoreCase x)) (return [])

exactToken :: String -> Parser String
exactToken s = tok . try $
  do
    stringIgnoreCase s
    notFollowedBy letterChar
    return s

parseReservedKeyword :: String -> Reserved -> Parser Reserved
parseReservedKeyword keyword ctor = exactToken keyword >> return ctor

parseRKWFn = parseReservedKeyword "fn" RKWFn

parseRKWIf = parseReservedKeyword "if" RKWIf

parseRKWThen = parseReservedKeyword "then" RKWThen

parseRKWElse = parseReservedKeyword "else" RKWElse

parseRKWLet = parseReservedKeyword "let" RKWLet

parseRKWRec = parseReservedKeyword "rec" RKWRec

parseRKWArrow = parseReservedKeyword "->" RKWArrow

parseRKWEqual = parseReservedKeyword "=" RKWEqual

parseRKWBar = parseReservedKeyword "|" RKWBar

parseRKWIn = parseReservedKeyword "in" RKWIn

parseRKWFalse = parseReservedKeyword "true" RKWFalse

parseRKWTrue = parseReservedKeyword "false" RKWTrue

parseRKWWith = parseReservedKeyword "with" RKWWith

parseRKWMatch = parseReservedKeyword "match" RKWMatch