module Main (main) where

import Control.Monad (when)
import GHC.IO.Handle (hFlush, hPutStr, isEOF)
import GHC.IO.Handle.FD (stderr)
import Lib
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = do
  args <- getArgs
  let argCount = length args

  when (argCount > 1) $ putStrLn "Usage: hslox [script]" >> exitWith (ExitFailure 64)
  if argCount == 1
    then runFile $ head args
    else runPrompt

runFile :: String -> IO ()
runFile path = readFile path >>= run

runPrompt :: IO ()
runPrompt = do
  hPutStr stderr "> " >> hFlush stderr
  end <- isEOF
  if end
    then return ()
    else do
      getLine >>= run >> runPrompt

run :: String -> IO ()
run input = putStrLn $ "Input Program: " ++ input
