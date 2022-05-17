module Eval where

import Ast
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map as M
import Debug.Trace
import Parser
import State

eval :: Monad m => InterpreteState -> Expr -> ExceptT String m Value
eval st expr = do
  case expr of
    Var s -> case M.lookup s (env st) of
      Nothing -> throwE $ "Unknown variable " <> s
      Just ex -> return ex
    Int n -> return $ VInt n
    Bool b -> return $ VBool b
    Binop op e1 e2 -> evalBinop op <$> eval st e1 <*> eval st e2
    Lambda f args -> return $ VClosure f args (env st)
    LetIn n body -> do
      (_, stt) <- evalDecl st n
      eval stt body
    IfThenElse cond then' else' -> do
      cond' <- eval st cond
      case cond' of
        VBool b ->
          if b
            then eval st then'
            else eval st else'
        _ -> throwE "Expect an boolean"
    App f arg -> do
      arg' <- eval st arg
      func <- eval st f
      case func of
        VClosure param body env' -> eval (st {env = M.insert param arg' env'}) body
        _ -> throwE "Applied non-function"

evalBinop :: Op -> Value -> Value -> Value
evalBinop op =
  case op of
    Add -> \(VInt a) (VInt b) -> VInt (a + b)
    Sub -> \(VInt a) (VInt b) -> VInt (a - b)
    Mul -> \(VInt a) (VInt b) -> VInt (a * b)
    Div -> \(VInt a) (VInt b) -> VInt (a `div` b)
    Eq -> \(VInt a) (VInt b) -> VBool (a == b)
    Lt -> \(VInt a) (VInt b) -> VBool (a < b)
    Leq -> \(VInt a) (VInt b) -> VBool (a <= b)
    -- UserDef -> \(VInt a) (VInt b) -> (App (App (Var "g") (Var "a")) (Var "b")) VBool (a <= b)

evalDecl :: Monad m => InterpreteState -> TopLevelCmd -> ExceptT String m (Value, InterpreteState)
evalDecl st expr = case expr of
  Let x e -> do
    val <- eval st e
    return (val, st {env = M.insert x val (env st)})
  LetRec f (Lambda x e') -> do
    let env' = M.insert f (VClosure x e' env') (env st)
    let val = VClosure x e' env'
    return (val, st {env = M.insert f val (env st)})
  _ -> undefined

evalCmd :: Monad m => Cmd -> ExceptT String (InterpreteStateT m) Value
evalCmd (CmdExpr e) = do
  st <- lift get
  eval st e
evalCmd (CmdLet e) = do
  st <- lift get
  (val, stt) <- evalDecl st e
  lift (put stt)
  return val

-- let rec fib n = if n < 3 then 1 else fib (n - 1) + fib (n - 2) in fib 4 ;;
