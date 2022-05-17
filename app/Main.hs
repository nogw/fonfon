module Main where

import Ast
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Char (isSpace)
import Eval
import Parser
import State
import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)

import Data.Map as Map

endOfInput :: String -> Bool
endOfInput = endOfInput' . reverse
  where
    endOfInput' str =
      case str of
        ';' : ';' : _ -> True
        c : str'
          | isSpace c -> endOfInput' str'
          | otherwise -> False
        _             -> False

addHistory :: String -> InputT IO ()
addHistory input = do
  history <- getHistory
  putHistory $ addHistoryUnlessConsecutiveDupe input history

readLine :: String -> InputT IO (Maybe String)
readLine input' = do
  let prompt = if Prelude.null input' then "# " else "  "
  userInput <- getInputLine prompt
  case userInput of
    Nothing -> return Nothing
    Just "quit" -> return Nothing
    Just "exit" -> return Nothing
    Just input | not (endOfInput input) -> do
      addHistory input
      readLine (input' ++ input ++ " ")
    Just input -> do
      addHistory input
      return (Just (input' ++ input))

repl :: InterpreteStateT (InputT IO) ()
repl = do
  userInput <- lift $ readLine ""
  case userInput of
    Nothing -> return ()
    Just input -> do
      ety <- runExceptT (processCmd input)
      case ety of
        Left m -> lift $ outputStrLn m
        Right m -> lift $ outputStrLn m
      repl
  where
    processCmd :: String -> ExceptT String (InterpreteStateT (InputT IO)) String
    processCmd input = do
      prog <- except (parseCmd input)
      v <- evalCmd prog
      return $ show v

haskelineSettings :: Settings IO
haskelineSettings = 
  Settings { complete       = completeFilename
           , historyFile    = Nothing
           , autoAddHistory = False
           }

evalExpr :: String -> InterpreteStateT IO ()
evalExpr input = do
  expr <- runExceptT (process input)
  case expr of
    Left s -> lift $ putStrLn s
    Right s -> lift $ putStrLn s
  where
    process :: Monad m => String -> ExceptT String (InterpreteStateT m) String
    process input = do
      prog <- except (parseCmd input)
      v <- evalCmd prog
      return $ show v

main :: IO ()
main = evalStateT (evalExpr "let plus_10 a = a + 10 in let (|>) v f = f v in 10 |> plus_10 ;;") initialState
-- main = evalStateT (evalExpr "let rec fib n = if n < 3 then 1 else fib (n - 1) + fib (n - 2) in fib 10;;") initialState