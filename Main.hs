module Main where

import Data.List.Split
import Data.List (isInfixOf)
import Data.List.Utils (replace)
import Data.Text (strip, unpack, pack)
import System.Posix.Process
import System.Posix.Types
import System.Posix.Directory
import System.Exit
import System.Directory
import Control.Monad
import System.IO.Error
import System.Console.Readline (readline, addHistory, setKeymap, getKeymapByName)
import System.Environment

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status {code :: StatusCode, pid :: Maybe ProcessID} deriving (Show)

-- |Built in commands available for execution.
builtinCmds :: [(String, [String] -> IO Status)]
builtinCmds = [("cd", hashCd), 
              ("help", hashHelp), 
              ("exit", hashExit),
              ("export", hashExport),
              ("printenv", hashPrintenv),
              ("bindkey", hashBindkey)]

-- |Top level entry-point for shell.
main :: IO ()
main = do initialize
          prompt

-- |Loops, continually prompting, until exit status is sent.
prompt :: IO ()
prompt = do
    dir <- getCurrentDirectory
    -- read and split input; replace environment vars
    input <- readline $ last (splitOn "/" dir) ++  " $ "
    case input of
      Nothing -> prompt
      Just i -> runCommand i

-- |Initialize the shell environment. Right now, only a few functions,
-- later, load from init dotfile.
initialize :: IO ()
initialize = do keymap <- getKeymapByName "vi"
                setKeymap keymap

-- |Execute a command after it has been readline'd by prompt.
runCommand :: String -> IO ()
runCommand line = do
    addHistory line -- add this line to the command history
    env <- getEnvironment
    let tok = (splitOn " " . replaceEnvVars env . unpack . strip . pack) line
    -- launch child process and keep pid
    status <- hashRun tok
    -- respond to child PIDs of different id's
    let responder s = 
            case s of
              Status {code=Prompt} -> prompt
              Status {code=Exit} -> return ()
              Status {code=Wait, pid=(Just childPid)} -> wait childPid responder
              Status {code=Wait, pid=Nothing} -> fail "Invalid status code."
    responder status


-- |Replace any instances of environment variables with their expanded
-- form.
replaceEnvVars :: [(String, String)] -> String -> String
replaceEnvVars env x = 
    foldl (\acc xs -> 
          if ("$" ++ fst xs) `isInfixOf` acc 
            then replace ("$" ++ fst xs) (snd xs) acc 
            else acc) x env

-- |Wait on the child PID. Handle any errors, then send status to continue
-- with prompt
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
hashRun :: [String] -> IO Status
hashRun [] = return Status {code = Prompt, pid = Nothing}
hashRun [""] = do putStrLn "No command provided."
                  return Status {code = Prompt, pid = Nothing}
hashRun (cmd:args) = 
    let builtin = lookup cmd builtinCmds
    in case builtin of
         Just b -> b args -- builtin exists
         Nothing -> execute cmd args -- no builtin exists

-- |Fork and exec a command with associated arguments. Return child PID.
execute :: String -> [String] -> IO Status
execute cmd args = 
    case args of
      [] -> handle cmd [] False
      [x] -> bgHandler [x]
      xs -> bgHandler xs
    where bgHandler xs = case last xs of
                           "&" -> handle cmd (init xs) True
                           _ -> handle cmd args False
          handle c a bg = do pid' <- forkExec c a
                             return $ pidHandler pid' bg

-- |Fork with defaults of: Command; Search PATH? true; Args; Environment of
-- nothing
forkExec :: String -> [String] -> IO ProcessID
forkExec cmd args = forkProcess (executeFile cmd True args Nothing)

-- |Change directories.
hashCd :: [String] -> IO Status
hashCd [arg] =
    do change <- tryIOError (changeWorkingDirectory arg)
       let status = Status {code = Prompt, pid = Nothing}
       case change of
         Left _ -> do putStrLn $ "cd: no such file or directory: " ++ arg
                      return status
         Right _ -> return status
hashCd _ = do putStrLn "Not a valid path in pwd."
              return Status {code = Prompt, pid = Nothing}

-- |Get help.
hashHelp :: [String] -> IO Status
hashHelp _ = do
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
hashExit _ = do
    putStrLn "Exiting..."
    return Status {code = Exit, pid = Nothing}

-- |Provide our own version of printenv that prints our own environment
-- variables.
hashPrintenv :: [String] -> IO Status
hashPrintenv [] = do env <- getEnvironment
                     mapM_ (\x -> putStrLn $ fst x ++ "=" ++ snd x) env
                     return Status {code = Prompt, pid = Nothing}
hashPrintenv _ = do putStrLn "Too many arguments passed to printenv."
                    return Status {code = Prompt, pid = Nothing}

-- |Export an environment variable.
hashExport :: [String] -> IO Status
hashExport [arg] = do let (x:y:_) = splitOn "=" arg
                      setEnv x y
                      return Status {code = Prompt, pid = Nothing}
hashExport _ = do putStrLn "Wrong number of arguments passed to export."
                  return Status {code = Prompt, pid = Nothing}

-- |Set emacs or vi keybindings mode.
hashBindkey :: [String] -> IO Status
hashBindkey [arg]
    | arg == "-v" = do keymap <- getKeymapByName "vi"
                       setKeymap keymap
                       return Status {code = Prompt, pid = Nothing}
    | arg == "-e" = do keymap <- getKeymapByName "emacs"
                       setKeymap keymap
                       return Status {code = Prompt, pid = Nothing}
hashBindkey _ = do putStrLn "Wrong number of arguments passed to export."
                   return Status {code = Prompt, pid = Nothing}

pidHandler :: ProcessID -> Bool -> Status
pidHandler pid' isBg
    | pid' == 0 || pid' == -1 = Status {code = Exit, pid = Nothing} -- child forked or error occurred, so exit
    | isBg = Status {code = Prompt, pid = Nothing}
    | not isBg = Status {code = Wait, pid = Just pid'} -- parent waits on child
pidHandler _ _ = Status {code = Exit, pid = Nothing} -- invalid case

