module Interpreter where 

import AbsFUN
import ErrM
import System.Exit (exitFailure)
import PrintFUN
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.List (find, findIndex, last, init)

data Result = Err String | Res (CExp, Environment) 
data CExp = CExp Exp (Map Ident CExp) deriving Show

failure :: (Show a) => a -> Result
failure x = Err $ "Error: " ++ show x

data Environment = Environment { funs :: Map Ident Exp
                               , callByValue :: Bool} deriving (Show, Eq)

interpret :: Program -> Bool -> Result
interpret (Prog defs) isCallByVal = 
  let envMap = Map.fromList(map transDef defs)
      env = Environment {funs=envMap, callByValue = isCallByVal}
      mainId = Ident "main"
  in case Map.lookup mainId (funs env) of
    Just funExp -> transCExp (addClosure funExp) env
    Nothing -> failure "no main found"

-- Make key value pair mappings for the funs field of environment
transDef :: Def -> (Ident, Exp)
transDef (DDef id args exp) = case args of
  []    -> (id, exp)
  args  -> let
    lastLambda = EAbs (last args) exp
    curry argId lambdaChain = EAbs argId lambdaChain 
    in (id, foldr curry lastLambda (init args))

transCExp :: CExp -> Environment -> Result
transCExp (CExp x closure) env = case x of
  EVar id             -> transVar id closure env
  EInt n              -> Res (CExp (EInt n) Map.empty, env)
  EApp exp1 exp2      -> case transCExp e1 env of
                            Err s -> Err s
                            Res (cexp, env) -> fApply cexp e2 env
                          where e1 = CExp exp1 closure; e2 = CExp exp2 closure
  EAdd exp1 exp2      -> arithmetic e1 e2 (+) env where e1 = CExp exp1 closure; e2 = CExp exp2 closure
  ESub exp1 exp2      -> arithmetic e1 e2 (-) env where e1 = CExp exp1 closure; e2 = CExp exp2 closure
  ELt exp1 exp2       -> arithmetic e1 e2 (ltInt) env where e1 = CExp exp1 closure; e2 = CExp exp2 closure
  EIf exp1 exp2 exp3  -> case i of
                           1 -> transCExp e2 env'
                           0 -> transCExp e3 env'
                         where Res (CExp (EInt i) _, env') = transCExp (CExp exp1 closure) env 
                               e2 = CExp exp2 closure
                               e3 = CExp exp3 closure
  EAbs id exp         -> Res (CExp (EAbs id exp) closure, env)

transVar :: Ident -> Map Ident CExp -> Environment -> Result 
transVar id closure env = case Map.lookup id closure of
  Just var -> Res (var, env)
  Nothing  -> case Map.lookup id (funs env) of
    Just lambda -> Res (CExp lambda Map.empty, env)
    Nothing  -> failure $ "unknown " ++ (show id)

addClosure :: Exp -> CExp
addClosure e = CExp e Map.empty

ltInt :: (Integral a) => a -> a -> a
ltInt a1 a2 = 
  let result = a1 < a2
  in case result of
    True -> 1
    False -> 0

fApply :: CExp -> CExp -> Environment -> Result
fApply func arg env = let 
  argVal = case callByValue env of 
    False -> Res (arg, env)
    True  -> transCExp arg env 
  in case argVal of
    Err s -> Err s
    -- the lambda's closure gets its id bound to the argument value
    Res (argExp, env) -> transCExp (bindArg func argExp) env

bindArg :: CExp -> CExp -> CExp
bindArg (CExp (EAbs id exp) closure) argExp = CExp exp $ insert id argExp closure

-- Take 2 expressions, function to apply, the environment, return the result
arithmetic :: CExp -> CExp -> (Integer -> Integer -> Integer) -> Environment -> Result
arithmetic e1 e2 operator env = let
  Res ((CExp valA _), newEnv) = transCExp e1 env
  Res ((CExp valB _), newEnv') = transCExp e2 newEnv
  in case (valA, valB) of 
    (EInt a, EInt b) -> Res (addClosure $ EInt (operator a b), newEnv') 
    otherwise        -> failure "Invalid argument to arithmetic expression"
