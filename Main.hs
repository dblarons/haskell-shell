module Main where

import Data.List.Split
import Data.List (isInfixOf)
import Data.List.Utils (replace)
import System.Posix.Process
import System.Posix.Types
import System.Posix.Directory
import System.Exit
import System.Directory
import Control.Monad
import System.IO.Error

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status {code :: StatusCode, pid :: Maybe ProcessID} deriving (Show)

-- |Type synonym for the environment structure.
type Env = [(String, String)]

-- |Built in commands available for execution.
builtinCmds :: [(String, [String] -> IO Status)]
builtinCmds = [("cd", hashCd), 
              ("help", hashHelp), 
              ("exit", hashExit)]

-- |Built in commands that require access to env.
builtinCmdsEnv :: [(String, [String] -> Env -> IO (Status, Env))]
builtinCmdsEnv = [("export", hashExport)]

-- |Entry point for shell. Start prompt with clean environment.
main :: IO ()
main = prompt []

-- |Top level prompt for shell. Loops until exit status is sent.
prompt :: Env -> IO ()
prompt env = do
    dir <- getCurrentDirectory
    -- prompt for input
    putStr (last (splitOn "/" dir) ++  " $ ")
    -- read and split input
    tok <- liftM (splitOn " " . replaceEnvVars env) getLine
    -- launch child process and keep pid
    (status, newEnv) <- hashRun tok env
    -- respond to child PIDs of different id's
    let responder s = 
            case s of
              Status {code=Prompt} -> prompt newEnv
              Status {code=Exit} -> return ()
              Status {code=Wait, pid=(Just childPid)} -> wait childPid responder
              Status {code=Wait, pid=Nothing} -> fail "Invalid status code."
    responder status

-- |Replace any instances of environment variables with their expanded
-- form.
replaceEnvVars :: Env -> String -> String
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
hashRun :: [String] -> Env -> IO (Status, Env)
hashRun [] _ = fail "Empty environment sent to hashRun."
hashRun (cmd:args) env = 
    let func = lookup cmd builtinCmds
    in case func of
         -- builtin exists, does not require ENV
         Just f -> do status <- f args
                      return (status, env)
         Nothing -> let envFunc = lookup cmd builtinCmdsEnv
                        in case envFunc of
                             -- builtin exists, requires ENV
                             Just f -> f args env
                             -- no builtin exists, run from PATH
                             Nothing -> do status <- execute cmd args env
                                           return (status, env)

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

-- |Export a new or updated entry to the shell environment. 
exportInsert :: (String, String) -> Env -> Env
exportInsert x@(var, path) env =
    case existing of
      Just _ -> map (\y -> if fst y == var then x else y) env
      Nothing -> env ++ [x]
    where existing = lookup var env

-- |Parse an export command and return a tuple describing the action: new,
-- append, or prepend.
exportParse :: String -> String -> (String, String)
exportParse x home = case parts of
                       [a, b] -> (a, b)
    where parts = map (replace "$HOME" home) $ splitOn "=" x

hashExport :: [String] -> Env -> IO (Status, Env)
hashExport [arg] env = do home <- getHomeDirectory
                          let p = exportParse arg home
                          return (Status {code = Prompt, pid = Nothing}, exportInsert p env)
hashExport _ env = do putStrLn "Wrong number of arguments passed to export."
                      return (Status {code = Prompt, pid = Nothing}, env)

-- |Fork and exec a command with associated arguments. Return child PID.
execute :: String -> [String] -> Env -> IO Status
execute cmd args env = 
    case args of
      [] -> handle cmd [] False
      [x] -> bgHandler [x]
      xs -> bgHandler xs
    where bgHandler xs = case last xs of
                           "&" -> handle cmd (init xs) True
                           _ -> handle cmd args False
          handle c a bg = do pid' <- forkExec c a env
                             return $ pidHandler pid' bg

-- |Fork with defaults of: Command; Search PATH? true; Args; Environment of
-- nothing
-- TODO: Search PATH here before executing a command. Prepend correct path
-- to command name.
forkExec :: String -> [String] -> Env -> IO ProcessID
forkExec cmd args env = forkProcess (executeFile cmd True args Nothing)

pidHandler :: ProcessID -> Bool -> Status
pidHandler pid' isBg
    | pid' == 0 || pid' == -1 = Status {code = Exit, pid = Nothing} -- child forked or error occurred, so exit
    | isBg = Status {code = Prompt, pid = Nothing}
    | not isBg = Status {code = Wait, pid = Just pid'} -- parent waits on child
pidHandler _ _ = Status {code = Exit, pid = Nothing} -- invalid case

