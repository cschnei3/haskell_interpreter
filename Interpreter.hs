module Interpreter where 
import AbsFUN
import ErrM
import System.Exit (exitFailure)
import PrintFUN
import Data.Map (Map)
import qualified Data.Map as Map
-- type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

-- interpret :: Program -> Result
interpret x = case x of
  Prog defs -> do
    let environment = map transDef defs
    in Map.fromList(environment)
    transExp environment.lookup("main") environment
    failure x

-- transDef :: Def -> Environment
transDef x, environment = case x of
  DDef id args exp  -> 
   (id, (args, exp)) 

-- transExp :: Exp -> Result
transExp x, environment = case x of
  EVar id  -> transIdent id
  EInt n  -> case n of 
    Integer i -> i
  EApp exp1 exp2  -> (transExp exp1) (transExp exp2)
  EAdd exp1 exp2  -> arithmetic exp1 exp2 (+)
  ESub exp1 exp2  -> arithmetic exp1 exp2 (-) 
  ELt exp1 exp2  -> arithmetic exp1 exp2 (<)
  EIf exp1 exp2 exp3  -> if transExp exp1 then transExp exp2 else transExp exp3
  EAbs id exp  -> failure x

-- transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> environment[id]

arithmetic e1 e2 operator 
    | isIntOrId e1 && isIntOrId e2 = operator (transExp e1) (transExp e2)
    | otherwise                    = failure x

isIntOrId x = case x of 
  EVar -> True
  EInt -> True
