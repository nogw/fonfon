module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Char (isLetter)
import Data.Functor (($>), (<&>))
import Data.Maybe (isJust)
import Data.Void
import Helpers
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseParens :: Parser Expr
parseParens = parens parseExpr

command :: Parser Cmd
command =
  parseToplevelLet
    <|> CmdExpr <$> parseExpr

operator :: Parser (Token [Char])
operator = oneOf ">|*="

operators' :: Parser String
operators' = (lexeme . try) $ many1 operator

parsePattern :: Parser Var
parsePattern = (lexeme . try) $ do
  pat <- parens operators' <|> many (alphaNumChar <|> char '_' <|> char '\'')
  check pat
  where
    check x =
      if x `elem` lowerKeywords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else return x

parseDecl :: Parser TopLevelCmd
parseDecl = do
  isRec <- isJust <$> optional parseRKWRec
  var <- parsePattern
  args <- many parseIdent
  body <- symbol "=" >> parseExpr
  return $
    if isRec
      then LetRec var (foldr Lambda body args)
      else Let var (foldr Lambda body args)

parseToplevelLet :: Parser Cmd
parseToplevelLet = do
  bind <- parseRKWLet >> parseDecl
  body <- optional (parseRKWIn >> parseExpr)
  case body of
    Nothing -> return $ CmdLet bind
    Just b' -> return $ CmdExpr (LetIn bind b')

parseVar :: Parser Expr
parseVar = Var <$> parseIdent

parseNumber :: Parser Integer
parseNumber = lexeme $ do
  int <- many1 digit
  float <- (try (char '.') >> ('.' :) <$> many1 digit) <|> pure ""
  let xs = int ++ float
  return $ read xs

parseBoolean :: Parser Bool
parseBoolean = False <$ parseRKWFalse <|> True <$ parseRKWTrue

parseBinaryOperation :: Parser Expr
parseBinaryOperation = makeExprParser parseConst operators
  where
    operators :: [[Operator Parser Expr]]
    operators =
      [ [InfixL (App <$ parseSpace)],
        [InfixL (Binop Add <$ symbol "+")],
        [InfixL (Binop Sub <$ symbol "-")],
        [InfixL (Binop Mul <$ symbol "*")],
        [InfixL (Binop Div <$ symbol "/")],
        [InfixL (Binop Eq <$ symbol "==")],
        [InfixL (Binop Leq <$ symbol "<=")],
        [InfixL (Binop Lt <$ symbol "<")],
        -- provisional, do i need another function to parse create-by-user operations?
        [ InfixL $ do
            op <- operators'
            pure (App . App (Var op))
        ]
      ]

parseLet :: Parser Expr
parseLet =
  LetIn <$> (parseRKWLet >> parseDecl) <*> (parseRKWIn >> parseExpr)

parseLambda :: Parser Expr
parseLambda = Lambda <$> (parseRKWFn >> parseIdent) <*> (symbol "->" >> parseExpr)

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  if_ <- parseRKWIf >> parseExpr
  then_ <- parseRKWThen >> parseExpr
  else_ <- parseRKWElse >> parseExpr
  return $ IfThenElse if_ then_ else_

parseConst :: Parser Expr
parseConst =
  parseParens
    <|> Int <$> parseNumber
    <|> Bool <$> parseBoolean
    <|> parseVar

parseExpr :: Parser Expr
parseExpr =
  parseIfThenElse
    <|> parseLet
    <|> parseLambda
    <|> parseBinaryOperation

parseCmd :: String -> Either String Cmd
parseCmd input =
  case parse (parseSpace >> command <* symbol ";;") "" input of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right c -> Right c