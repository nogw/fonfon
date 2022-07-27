{-# LANGUAGE LambdaCase #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Char (isLetter)
import Data.Functor (($>), (<&>))
import Data.Maybe (isJust)
import Data.Void
import Helpers
import Ourlude
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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

parseDeclName :: Parser Var
parseDeclName = (lexeme . try) $ do
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
  var <- parseDeclName
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

parseParenExpr :: Parser Expr
parseParenExpr = do
  items <- parens $ parseExpr `sepBy` symbol ","
  case items of
    [x] -> return x
    _ -> return $ Tuple items

parseBracketExpr :: Parser Expr
parseBracketExpr = do
  items <- brackets $ parseExpr `sepBy` symbol ","
  case items of
    [] -> return Nil
    _ -> return $ foldr Cons Nil items

parsePattern :: Parser Pattern
parsePattern = makeExprParser atomPattern table
  where
    table :: [[Operator Parser Pattern]]
    table = [[InfixR (PCons <$ symbol "::")]]

parseParenPattern :: Parser Pattern
parseParenPattern = do
  elems <- parens $ sepBy parsePattern (symbol ",")
  case elems of
    [x] -> return x
    _ -> return $ PTuple elems

parseBracketPattern :: Parser Pattern
parseBracketPattern = do
  elems <- brackets $ sepBy parsePattern (symbol ",")
  case elems of
    [] -> return PNil
    _ -> return $ foldr PCons PNil elems

parseBinaryOperation :: Parser Expr
parseBinaryOperation = makeExprParser atomExpr operators
  where
    operators :: [[Operator Parser Expr]]
    operators =
      [ [InfixL (App <$ parseSpace)],
        [InfixL (Binop Add <$ symbol "+")],
        [InfixL (Binop Sub <$ symbol "-")],
        [InfixL (Binop Mul <$ symbol "*")],
        [InfixL (Binop Div <$ symbol "/")],
        [InfixL (Binop Eq <$ symbol "=")],
        [InfixL (Binop Leq <$ symbol "<=")],
        [InfixL (Binop Lt <$ symbol "<")],
        [InfixR (Cons <$ symbol "::")],
        [ InfixL $ do
            op <- operators'
            pure (App . App (Var op))
        ]
      ]

parseLet :: Parser Expr
parseLet =
  LetIn <$> (parseRKWLet >> parseDecl) <*> (parseRKWIn >> parseExpr)

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  if_ <- parseRKWIf >> parseExpr
  then_ <- parseRKWThen >> parseExpr
  else_ <- parseRKWElse >> parseExpr
  return $ IfThenElse if_ then_ else_

parseLambda :: Parser Expr
parseLambda = Lambda <$> (parseRKWFn >> parseIdent) <*> (symbol "->" >> parseExpr)

parseMatch :: Parser Expr
parseMatch = do
  val <- parseRKWMatch >> parseExpr
  cases <- parseRKWWith >> matchCases
  return $ Match val cases

matchCases :: Parser [(Pattern, Expr)]
matchCases = do
  first <- optional parseRKWBar >> matchClause
  rest <- many (parseRKWBar >> matchClause)
  return (first : rest)
  where
    matchClause :: Parser (Pattern, Expr)
    matchClause = (,) <$> (parsePattern <* symbol "->") <*> parseExpr

atomExpr :: Parser Expr
atomExpr =
  -- parseParens <|>
  Int <$> parseNumber
    <|> Bool <$> parseBoolean
    <|> parseVar
    <|> parseParenExpr
    <|> parseBracketExpr

atomPattern :: Parser Pattern
atomPattern =
  PInt <$> parseNumber
    <|> PBool <$> parseBoolean
    <|> PVar <$> parseIdent
    <|> parseParenPattern
    <|> parseBracketPattern

parseExpr :: Parser Expr
parseExpr =
  parseIfThenElse
    <|> parseLet
    <|> parseLambda
    <|> parseMatch
    <|> try parseBinaryOperation

parseCmd :: String -> Either String Cmd
parseCmd input =
  case parse (parseSpace >> command <* symbol ";;") "" input of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right c -> Right c

parseFile :: FilePath -> IO (Either String [Cmd])
parseFile filename = do
  content <- readFile filename
  parserFileExpr content filename |> \case
    Left err -> return $ Left err
    Right ast -> return $ Right ast
  where
    parserFileExpr input filename =
      parse (between parseSpace eof (many $ command <* symbol ";;")) filename input |> \case
        Left err -> Left $ errorBundlePretty err
        Right output -> Right output