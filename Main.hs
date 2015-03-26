module Main where

import Data.List.Split
import Data.Maybe
import System.Posix.Process
import System.Posix.Types
import System.Posix.Directory
import System.Exit
import System.Directory
import Control.Monad

-- |Built in commands available for execution.
builtinCmds :: [(String, [String] -> IO ProcessID)]
builtinCmds = [("cd", hashCd), 
              ("help", hashHelp), 
              ("exit", hashExit)]

-- |Main loop for shell execution.
main :: IO ()
main = do
    dir <- getCurrentDirectory
    -- prompt for input
    putStr (last (splitOn "/" dir) ++  " $ ")
    -- read and split input
    tok <- liftM (splitOn " ") getLine
    -- launch child process and keep pid
    childPID <- builtin tok
    -- respond to child PIDs of different id's
    case childPID of
      0 -> main
      -1 -> return ()
      _ -> wait childPID

-- |Wait on the child PID. Handle any errors, then continue with main.
wait :: ProcessID -> IO ()
wait childPID = do
    status <- waitForChild childPID
    case status of
      Exited ExitSuccess -> main
      x -> main

-- |Run builtin command if it exists, otherwise run from PATH.
builtin :: [String] -> IO ProcessID
builtin (cmd:args) = 
    let func = lookup cmd builtinCmds
    in case func of
         Just f -> f args
         Nothing -> execute cmd args

-- |Change directories.
hashCd :: [String] -> IO ProcessID
hashCd [arg] = do
    changeWorkingDirectory arg
    return 0

-- |Get help.
hashHelp :: [String] -> IO ProcessID
hashHelp args = do
    putStrLn "------------------------"
    putStrLn "HASH -- Haskell, A SHell"
    putStrLn "Author: Aaron Smith"
    putStrLn "------------------------\n"
    putStrLn "Builtins:"
    mapM_ (putStrLn . fst) builtinCmds
    putStrLn ""
    return 0

-- |Exit the shell
hashExit :: [String] -> IO ProcessID
hashExit args = do
    putStrLn "Exiting..."
    return (-1)

-- |Fork and exec a command with associated arguments. Return child PID.
execute :: String -> [String] -> IO ProcessID
execute cmd args = forkProcess (executeFile cmd True args Nothing)

-- |Make this thread wait for a child thread with pid childPID.
waitForChild :: ProcessID -> IO ProcessStatus
waitForChild childPID = 
    do status <- getProcessStatus True False childPID
       case status of
         Nothing -> fail "Error: Nothing from getProcessStatus"
         Just ps -> return ps

