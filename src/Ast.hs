module Ast where 

import Data.Map as M

type Var = String 

type Env = M.Map String Value

data Cmd
  = CmdExpr Expr 
  | CmdLet TopLevelCmd
  deriving Show

data TopLevelCmd 
  = Let Var Expr 
  | LetRec Var Expr
  deriving Show

data Value 
  = VInt Integer 
  | VBool Bool
  | VClosure Var Expr Env
  deriving Show

data Expr 
  = Var Var 
  | Int Integer
  | Bool Bool
  | Tuple [Expr]
  | Nil
  | Neg Expr
  | Cons Expr Expr
  | Lambda Var Expr
  | LetIn TopLevelCmd Expr
  | App Expr Expr
  | Binop Op Expr Expr
  | IfThenElse Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  deriving Show

data Pattern 
  = PInt Integer 
  | PBool Bool
  | PVar Var 
  | PTuple [Pattern]
  | PNil 
  | PCons Pattern Pattern
  deriving Show

data Op 
  = Add 
  | Sub 
  | Mul 
  | Div 
  | Eq
  | Lt
  | Leq
  deriving Show
