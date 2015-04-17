module Main where

import Data.List.Split
import Data.Maybe
import System.Posix.Process
import System.Posix.Types
import System.Posix.Directory
import System.Exit
import System.Directory
import Control.Monad
import System.IO.Error
import Data.Typeable

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status {code :: StatusCode, pid :: Maybe ProcessID} deriving (Show)

-- |Built in commands available for execution.
builtinCmds :: [(String, [String] -> IO Status)]
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
    status <- builtin tok
    -- respond to child PIDs of different id's
    let responder status = 
            case status of
              Status {code=Prompt} -> main
              Status {code=Exit} -> return ()
              Status {code=Wait, pid=(Just childPid)} -> wait childPid responder
    responder status

-- |Wait on the child PID. Handle any errors, then continue with main.
wait :: ProcessID -> (Status -> IO ()) -> IO ()
wait childPID responder = do
    -- wait for child
    status <- getProcessStatus True False childPID
    -- unpack status; fail if status not present;
    case status of
      Nothing -> fail "Error: Nothing from getProcessStatus"
      Just ps -> case ps of
                   Exited ExitSuccess ->
                    responder Status {code = Prompt, pid = Nothing} -- exited normally
                   _ -> responder Status {code = Prompt, pid = Nothing}

-- |Run builtin command if it exists, otherwise run from PATH.
builtin :: [String] -> IO Status
builtin (cmd:args) = 
    let func = lookup cmd builtinCmds
    in case func of
         Just f -> f args
         Nothing -> execute cmd args

-- |Change directories.
hashCd :: [String] -> IO Status
hashCd [arg] =
    do change <- tryIOError (changeWorkingDirectory arg)
       let status = Status {code = Prompt, pid = Nothing}
       case change of
         Left e -> do putStrLn $ "cd: no such file or directory: " ++ arg
                      return status
         Right retCode -> return status
hashCd _ = do putStrLn "Not a valid path in pwd."
              return Status {code = Prompt, pid = Nothing}

-- |Get help.
hashHelp :: [String] -> IO Status
hashHelp args = do
    putStrLn "------------------------"
    putStrLn "HASH -- Haskell, A SHell"
    putStrLn "Author: Aaron Smith"
    putStrLn "------------------------\n"
    putStrLn "Builtins:"
    mapM_ (putStrLn . fst) builtinCmds
    putStrLn ""
    return Status {code = Prompt, pid = Nothing}

-- |Exit the shell
hashExit :: [String] -> IO Status
hashExit args = do
    putStrLn "Exiting..."
    return Status {code = Exit, pid = Nothing}

-- |Fork and exec a command with associated arguments. Return child PID.
execute :: String -> [String] -> IO Status
execute cmd args = do
    pid <- forkProcess (executeFile cmd True args Nothing)
    case pid of
      0 -> exit -- child forked, so exit
      -1 -> exit -- error occurred; exit
      _ -> return Status {code = Wait, pid = Just pid} -- parent waits on child
    where exit = return Status {code = Exit, pid = Nothing}

