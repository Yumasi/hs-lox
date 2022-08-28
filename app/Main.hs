module Main (main) where

import Control.Monad (when)
import Control.Monad.State (evalState, execState, State, put)
import Lib()
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, hPutStr, hPutStrLn, isEOF, stderr)

type InterpreterError = Bool

main :: IO ()
main = do
  args <- getArgs
  let argCount = length args

  when (argCount > 1) $ putStrLn "Usage: hslox [script]" >> exitWith (ExitFailure 64)
  if argCount == 1
    then runFile $ head args
    else runPrompt

runFile :: FilePath -> IO ()
runFile path = do
  fileContent <- readFile path
  let hasError = execState (run fileContent) False
  when hasError $ exitWith (ExitFailure 65)

runPrompt :: IO ()
runPrompt = do
  hPutStr stderr "> " >> hFlush stderr
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      evalState (run line) False >> runPrompt

run :: String -> State InterpreterError (IO ())
run input = return $ putStrLn $ "Input Program: " ++ input

error :: Int -> String -> State InterpreterError (IO ())
error line message = do
  put True
  return $ report ""

  where
    report :: String -> IO ()
    report loc = hPutStrLn stderr $ "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ message
