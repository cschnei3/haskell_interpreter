module Interpreter where 
import AbsFUN
import ErrM
import System.Exit (exitFailure)
import PrintFUN
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x



interpret :: Program -> IO ()
interpret x = case x of
  Program defs -> do
    let y = transProgram defs;
    print defs;
    exitFailure

transProgram :: Program -> Result
transProgram x = case x of
  Prog defs  -> failure x


transDef :: Def -> Result
transDef x = case x of
  DDef id ids exp  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EVar id  -> failure x
  EInt n  -> failure x
  EApp exp1 exp2  -> failure x
  EAdd exp1 exp2  -> failure x
  ESub exp1 exp2  -> failure x
  ELt exp1 exp2  -> failure x
  EIf exp1 exp2 exp3  -> failure x
  EAbs id exp  -> failure x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x
