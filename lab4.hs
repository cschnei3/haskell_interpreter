module Main where

import AbsFUN
import ErrM
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified LexFUN 
import ParFUN
import ErrM

import Interpreter

check :: Bool -> String -> IO ()
check isCallByVal s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok tree -> do
      case interpret tree isCallByVal of 
        Res (EInt i, _) -> do putStrLn (show i)
        Err err -> do putStrLn err 
                      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [callMode, file] -> let 
      isCallByVal = case callMode of 
        "-v" -> True
        "-n" -> False
      in readFile file >>= (check isCallByVal)
    [file] -> readFile file >>= (check True)
    _      -> do
      putStrLn "Usage: lab4 <SourceFile> [-n|-v]"
      exitFailure
  

