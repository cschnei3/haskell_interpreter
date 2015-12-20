module Interpreter where 
import AbsFUN
import ErrM
import System.Exit (exitFailure)
import PrintFUN
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Int (Int)
import qualified Data.Int as Int
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

data Environment = Environment { funs :: Map String Exp
                               , curFun :: String
                               , vals :: Map String Int
                               , callByValue :: Bool}

-- interpret :: Program -> Result
interpret x = case x of
  Prog defs -> do
    let envMap = Map.fromList(map transDef defs)
        env = Environment {funs=envMap, curFun="main", vals = Map.empty, callByValue = False}
    -- fApply "main" Nothing env
    failure x

-- transDef :: Def -> Environment
transDef x env = case x of
  DDef id args exp  -> (id, exp) 

-- transExp :: Exp -> Result
transExp x env = case x of
  EVar id  -> transIdent id
  EInt n  -> n
  EApp exp1 exp2      -> fApply env
  EAdd exp1 exp2      -> arithmetic exp1 exp2 (+)
  ESub exp1 exp2      -> arithmetic exp1 exp2 (-) 
  ELt exp1 exp2       -> arithmetic exp1 exp2 (<)
  EIf exp1 exp2 exp3  -> if transExp exp1 then transExp exp2 else transExp exp3
  EAbs id exp         -> failure x

-- transIdent :: Ident -> Result
transIdent x env = case x of
  Ident str  -> case env.funs.lookup(curFun) of
    Just j -> j
    Nothing n -> env

-- fApply :: String -> Args -> Environment -> (Result, Environment)
fApply id env = "fail"
--let 
--  funExp = env.funs Map.! env.curFun
--  vals = env.

arithmetic e1 e2 operator env
    | isIntOrId e1 && isIntOrId e2 = operator (transExp e1 env) (transExp e2 env)
    | otherwise                    = failure "fail"

isIntOrId x = case x of 
  EVar _ -> True
  EInt _ -> True
  otherwise -> False
