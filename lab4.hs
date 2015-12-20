module Main where

import AbsFUN
import ErrM
import System.Environment (getArgs)
import System.Exit (exitFailure)
import LexFUN
import ParFUN
import ErrM

import Interpreter

check :: String -> String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> do
      let y = interpret tree
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab4 <SourceFile>"
      exitFailure
  

