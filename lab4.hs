module Main where

import AbsFUN
import ErrM
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified LexFUN 
import ParFUN
import ErrM

import Interpreter

check :: String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok tree -> do
      case interpret tree True of 
        Res (EInt i, _) -> do putStrLn (show i)
                              exitFailure
        Err err -> do putStrLn err 
                      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab4 <SourceFile>"
      exitFailure
  

