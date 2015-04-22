module Main where

import Data.List.Split
import Data.List (isInfixOf)
import Data.List.Utils (replace)
import Data.Text (strip, unpack, pack)
import System.Posix.Process
import System.Posix.Types
import System.Posix.Directory
import System.Posix.IO
import System.Exit
import System.Directory
import System.IO.Error
import System.IO
import System.Console.Readline (readline, addHistory, setKeymap, getKeymapByName)
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base (catch, IOException)

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status {code :: StatusCode, pid :: Maybe ProcessID} deriving (Show)

-- |A global list of FDs that should be closed in the client.
type CloseFDs = MVar [Fd]

-- |Should command be run in the background?
type Background = Bool

-- |Type for a parsed user command. Tuples represent piped data.
data Pipeline a = Cmd a | Pipe (Pipeline a) (Pipeline a) deriving (Show, Eq)

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

-- |Parse an input string into a recursive Pipeline data structure.
pipeParser :: String -> Pipeline String
pipeParser str = toTree $ splitOn "|" str :: Pipeline String
    where toTree cmds = case cmds of
                          [] -> Cmd "" -- invalid case
                          [x] -> Cmd (unpack . strip . pack $ x)
                          (x:xs) -> Pipe (toTree [x]) (toTree xs)

-- |Check if command should be backgrounded or not.
-- Return str unchanged if no & found; return without & if & found.
backgroundParser :: String -> (String, Background)
backgroundParser str = let cleanStr = (unpack . strip . pack) str
                       in case last cleanStr of
                            '&' -> (init cleanStr, True)
                            _ -> (str, False)

-- |Execute a command after it has been readline'd by prompt.
runCommand :: String -> IO ()
runCommand line = do
    addHistory line -- add this line to the command history
    env <- getEnvironment

    -- Strip, then split on pipe operators.
    let cmds = (splitOn "|" . unpack . strip . pack) line

    -- Strip line, replace environment variables, then split into args.
    let tok = (splitOn " " . replaceEnvVars env . unpack . strip . pack) line

    -- Initialize a list of FDs to close.
    closefds <- newMVar []

    -- launch child process and keep pid
    status <- hashRun tok closefds ""
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
hashRun :: [String] -> CloseFDs -> String -> IO Status
hashRun [] _ _ = return Status {code = Prompt, pid = Nothing}
hashRun [""] _ _ = do putStrLn "No command provided."
                      return Status {code = Prompt, pid = Nothing}
hashRun (cmd:args) closefds input =
    let builtin = lookup cmd builtinCmds
    in case builtin of
         Just b -> b args -- builtin exists
         Nothing -> execute cmd args closefds input -- no builtin exists

-- |Fork and exec a command with associated arguments. Return child PID.
-- Take a list of file descriptors needing to be closed and a string of
-- input to that command.
execute :: String -> [String] -> CloseFDs -> String -> IO Status
execute cmd args closefds input =
    do (stdinread, stdinwrite) <- createPipe
       (stdoutread, stdoutwrite) <- createPipe

       -- Add parent FDs to close list because they must always be closed
       -- in the children.
       addCloseFDs closefds [stdinwrite, stdoutread]

       -- Fork the child, decide whether it should be run in background.
       status <- withMVar closefds (\fds -> bgHandler fds stdinread stdoutwrite) :: IO Status

       -- Close client-side FDs in parent.
       closeFd stdinread
       closeFd stdoutwrite

       -- Write the input to the command.
       stdinhdl <- fdToHandle stdinwrite
       forkIO $ do hPutStr stdinhdl input
                   hClose stdinhdl

       -- Prepare to receive output from the command.
       stdouthdl <- fdToHandle stdoutread

       return status

        -- TODO: Replace this with code that correctly handles backgrounding.
        where bgHandler fds stdinread stdoutwrite =
               case args of
                 [] -> do p <- forkExec cmd [] fds stdinread stdoutwrite
                          return $ pidHandler False p :: IO Status
                 xs ->
                    case last xs of
                      "&" -> do p <- forkExec cmd (init xs) fds stdinread stdoutwrite
                                return $ pidHandler True p :: IO Status
                      _ -> do p <- forkExec cmd xs fds stdinread stdoutwrite
                              return $ pidHandler False p :: IO Status

-- |Add FDs to list of FDs that must be closed in a child after a fork
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- |Given a pid and background flag, return status code
pidHandler :: Bool -> ProcessID -> Status
pidHandler isBg pid'
    | pid' == 0 || pid' == -1 = Status {code = Exit, pid = Nothing} -- child forked or error occurred, so exit
    | isBg = Status {code = Prompt, pid = Nothing}
    | not isBg = Status {code = Wait, pid = Just pid'} -- parent waits on child
pidHandler _ _ = Status {code = Exit, pid = Nothing} -- invalid case

-- |Fork with defaults of: Command; Search PATH? true; Args; Environment of
-- nothing
forkExec :: String -> [String] -> [Fd] -> Fd -> Fd -> IO ProcessID
forkExec cmd args closefds stdinread stdoutwrite =
    do -- connect these pipes to standard I/O
       dupTo stdinread stdInput
       dupTo stdoutwrite stdOutput

       -- Close original pipe FDs
       closeFd stdinread
       closeFd stdoutwrite

       -- Close open FDs that were inherited from the parent.
       mapM_ (\fd -> catch (closeFd fd) (\e -> do let err = show (e :: IOException)
                                                  putStrLn err
                                                  return ())) closefds

       forkProcess (executeFile cmd True args Nothing)

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

