module Interpreter where 

import AbsFUN
import ErrM
import System.Exit (exitFailure)
import PrintFUN
--import Debug.Trace (trace)
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.List (find, findIndex, last, init)
-- import Data.Int (Int)
-- import qualified Data.Int as Int

-- dummy function to restore referential transparency and hide logging
-- when not debugging
trace :: String -> a -> a
trace a b = b

data Result = Err String | Res (Exp, Environment)
failure :: (Show a) => a -> Result
failure x = Err $ "Undefined case: " ++ show x

data Environment = Environment { funs :: Map Ident ([Ident], Exp)
                               , vars :: [Map Ident Exp]
                               , callByValue :: Bool} deriving (Eq)

interpret :: Program -> Bool -> Result
interpret (Prog defs) isCallByVal = 
  let envMap = Map.fromList(map transDef defs)
      env = Environment {funs=envMap, vars = [], callByValue = isCallByVal}
      mainId = trace ("calling main") Ident "main"
  in case Map.lookup mainId (funs env) of
    Just (_, funExp) -> transExp funExp env
    Nothing -> failure "no main found"

-- Make key value pair mappings for the funs field of environment
transDef :: Def -> (Ident, ([Ident], Exp))
transDef (DDef id args exp) = (id, (args, exp)) 

transExp :: Exp -> Environment -> Result
transExp x env = trace ("translating exp " ++ (show x)) $ case x of
  EVar id             -> transVar id env
  EInt n              -> Res (EInt n, env)
  -- exp1 is the fun name, exp2 is the 
  EApp exp1 exp2      -> fApply exp1 exp2 env
  EAdd exp1 exp2      -> arithmetic exp1 exp2 (+) env
  ESub exp1 exp2      -> arithmetic exp1 exp2 (-) env
  ELt exp1 exp2       -> arithmetic exp1 exp2 (ltInt) env
  EIf exp1 exp2 exp3  -> case transExp exp1 env of
                           Res r -> case fst r of
                             EInt 1 -> transExp exp2 env
                             EInt 0 -> transExp exp3 env
  EAbs id exp         -> failure "not implemented"

--lambda :: Ident -> Exp -> Result 
--lambda id exp = let


transVar :: Ident -> Environment -> Result 
transVar id env = case 
  do  varScope  <- find (Map.member id) (reverse $ vars env) 
      Map.lookup id varScope of
      Just var -> Res (var, env)
      Nothing  -> let 
        funLookup = Map.lookup id (funs env)
        in case funLookup of
          Just (_, exp) -> Res (exp, env)
          Nothing  -> failure "ident does not exist"

ltInt :: (Integral a) => a -> a -> a
ltInt a1 a2 = 
  let result = a1 < a2
  in case result of
    True -> 1
    False -> 0

-- Function calls only take one expression because
-- They are defined recursively
-- ie mult a b c will result in 3 function calls
-- EApp EApp EApp EVar
fApply :: Exp -> Exp -> Environment -> Result
fApply (EVar e1) e2 env = let
  newEnv = trace ("fApply: e1: " ++ (show e1) ++ " e2: " ++ (show e2)) bindArgs e1 e2 (pushScope env)
  funLookup = Map.lookup e1 (funs env)
  in case newEnv of
    Nothing     -> failure "Called non-existant function"
    Just newEnv -> case funLookup of
      Nothing           -> failure "Called non-existant function"
      Just (_, funExp)  -> let Res (res, appliedEnv) = transExp funExp newEnv
                           in Res (res, popScope appliedEnv)

fApply e1 e2 env = trace ("e1: " ++ (show e1) ++ " e2: " ++ (show e2)) failure "first expression of application must be an id"
  
-- Return an environment with a new scope set up for the function call
bindArgs :: Ident -> Exp -> Environment -> Maybe Environment
bindArgs id exp env = let
  argIds = trace("binding args: " ++ (show exp)) Map.lookup id (funs env) 
  in case argIds of
    Nothing -> Nothing
    Just (argIds, _) -> case exp of
      -- > 1 arg remaining
      (EApp headArg tailArgs) -> Just $ bindArgs' argIds (EApp headArg tailArgs) env 
      -- single arg. EApp's always have an argument, otherwise it is a var
      arg -> trace("calling bindArgs'' with " ++ (show $ argIds!!0) ++ " " ++ (show arg)) $ Just $ bindArgs'' (argIds!!0) arg env

-- Recursive case for > 1 argument
bindArgs' :: [Ident] -> Exp -> Environment -> Environment
bindArgs' (argHead:argList) (EApp head tail) env = let
  -- handle the current arg
  newEnv = trace("in bindArgs") bindArgs'' argHead head env
  in case tail of 
    EApp e1 e2 -> bindArgs' argList (EApp e1 e2) newEnv
    -- argList must always be the same size as the recursive EApp linked list
    e -> bindArgs'' (argList !! 0) e newEnv
    
-- in call by value the expression is evaluated, which will result in some value
-- in call by name, we subtitute binding the value of the expression
-- for simply the expression itself as the variable
bindArgs'' :: Ident -> Exp -> Environment -> Environment
bindArgs'' argId argVal env = let 
  varVal = case (callByValue env) of 
    True  -> let Res (exp, _) = trace("argExp: " ++ (show argVal)) transExp argVal env
             in trace("WHY?!") exp
    False -> trace ("you fckin wot") argVal
  in  let newVars = trace ("varVal value is " ++ (show varVal)) $ updateVar argId varVal (vars env)
      in trace("newVars: " ++ (show newVars)) env { vars = newVars } 

updateVar :: Ident -> Exp -> [Map Ident Exp] -> [Map Ident Exp]
updateVar id var varScopes = (init varScopes) ++ [insert id var (last varScopes)]
--  scopeIndex        <- findIndex (Map.member id) varScopes
--  (before,_: after) <- splitAt (scopeIndex+1) varScopes
--  before ++ [var] ++ after

pushScope :: Environment -> Environment
pushScope env = env { vars = (vars env) ++ [Map.empty] }

popScope :: Environment -> Environment
popScope env = env { vars = init (vars env) }

-- Take 2 expressions, function to apply, the environment, return the result
arithmetic :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Environment -> Result
arithmetic e1 e2 operator env = let
  Res (valA, newEnv) = transExp e1 env--getIntExp e1 env
  Res (valB, newEnv') = transExp e2 newEnv
  in case (valA, valB) of 
    (EInt a, EInt b) -> trace ("applying operator to " ++ (show a) ++ " " ++ (show b) ++ " result " ++ (show $ operator a b)) Res (EInt (operator a b), newEnv') 
    otherwise        -> failure "Invalid argument to arithmetic expression"
  --  Just r  -> r
  --  Nothing -> failure "Invalid argument to arithmetic expression"

{-getIntExp :: Exp -> Environment -> Maybe Integer
getIntExp (EVar id) env = do 
  exp <- getVar id env
  case transExp exp env of
    (EInt i) -> i

  getIntExp (EInt i) env = Just i
getIntExp e env = getIntExp (transExp e env) env-}

{-isIntOrId :: Exp -> Bool
  isIntOrId x = case x of 
  EVar _ -> True
  EInt _ -> True
  otherwise -> False-}

{-buildCallList exp = case exp of
  (EApp head tail) = [head] ++ (buildCallList tail) 
  exp              = [head]-}
