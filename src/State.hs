module State where 
import Control.Monad.Trans.State.Lazy
import Ast 
import Data.Map as M 

data InterpreteState = State 
  {
    freshId :: Int, 
    env :: Env
  }
  deriving Show

type InterpreteStateT = StateT InterpreteState 

initialState :: InterpreteState
initialState = State 
  {
    freshId = 0,
    env = M.empty
  }
