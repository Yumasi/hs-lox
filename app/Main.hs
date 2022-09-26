{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (Text)
import Control.Monad (when)
import Control.Monad.State (evalStateT, runStateT, StateT, put, lift)
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
  fileContent <- T.readFile path
  (_, hasError) <- runStateT (run fileContent) False
  when hasError $ exitWith (ExitFailure 65)

runPrompt :: IO ()
runPrompt = do
  hPutStr stderr "> " >> hFlush stderr
  end <- isEOF
  if end
    then return ()
    else do
      line <- T.getLine
      evalStateT (run line) False >> runPrompt

run :: Text -> StateT InterpreterError IO ()
run input = lift $ T.putStrLn $ "Input Program: " <> input

interpreterError :: Int -> String -> StateT InterpreterError IO ()
interpreterError line message = do
  put True
  lift $ report ""

  where
    report :: String -> IO ()
    report loc = hPutStrLn stderr $ "[line " ++ show line ++ "] Error" ++ loc ++ ": " ++ message
