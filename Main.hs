{-# LANGUAGE FlexibleInstances #-}
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

-- |A global list of FDs that should be closed in the client.
type CloseFDs = MVar [Fd]

-- |Should command be run in the background?
type Background = Bool

-- |Type for a parsed user command. Tuples represent piped data.
data Pipeline a = Cmd a | HFile a | Pipe (Pipeline a) (Pipeline a) deriving (Show, Eq)

-- |Result of running a command.
data CommandResult = CommandResult {
                   cmdOutput :: IO String, -- output of a command
                   getExitStatus :: IO ProcessStatus -- exit status of command
                   }

-- |Class for anything that is a runnable command.
class CommandLike a where
    -- |Given a command a input String, invoke the command.
    invoke :: a -> CloseFDs -> String -> IO CommandResult

-- |Built in commands available for execution.
builtinCmds :: [(String, [String] -> IO String)]
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
    -- read and split input
    input <- readline $ last (splitOn "/" dir) ++  " $ "
    case input of
      Nothing -> prompt
      Just i -> runCommand $ (unwords . words) i

-- |Initialize the shell environment. Right now, only a few functions,
-- later, load from init dotfile.
initialize :: IO ()
initialize = do home <- getHomeDirectory
                exists <- doesFileExist $ replaceTilde home "~/.hashrc"
                if exists
                  then do f <- readFile $ replaceTilde home "~/.hashrc"
                          parseDotfile f
                  else do keymap <- getKeymapByName "vi"
                          setKeymap keymap

parseDotfile :: String -> IO ()
parseDotfile s = 
    do foldl (\x y -> initLine x (words y)) (return []) (lines s)
       return ()
    where initLine x l@(y:ys) = case y of
                                  "bindkey" -> hashBindkey ys
                                  "export" -> hashExport ys
                                  _ -> fail $ "Unknown line in .hashrc: " ++ unwords l


-- |Make a Pipeline runnable as a command.
instance CommandLike (Pipeline String) where
    -- Deconstruct pipe into source and destination, pipe data between
    -- them, and return a common exit code.
    invoke (Pipe src dest) closefds input =
      do res1 <- invoke src closefds input
         output1 <- cmdOutput res1 -- output of first cmd
         sec <- getExitStatus res1 -- unwrap (i.e. force evaluate) first command
         -- Wait for first command to complete successfully before starting
         -- second command. Otherwise a race condition for FDs ensues.
         case sec of
           Exited ExitSuccess -> do res2 <- invoke dest closefds output1
                                    dec <- getExitStatus res2
                                    return $ CommandResult (cmdOutput res2) (return dec)
           x -> return $ CommandResult (return []) (return x)
    -- Redirect input into file.
    invoke (HFile name) closefds input =
        do writeFile name input
           return $ CommandResult (return []) (return (Exited ExitSuccess))
    -- Unwrap Cmd and call invoke on the command it contains. invoke
    -- differently for builtin command or external.
    invoke (Cmd src) closefds input = 
        let (cmd:args) = words src
            builtin = lookup cmd builtinCmds
        in case builtin of
             Just b -> invoke b closefds (unwords args) -- invoke builtin
             Nothing -> invoke (cmd, args) closefds input

-- |Make a builtin runnable act as a command. Takes args string as input.
instance CommandLike ([String] -> IO String) where
    invoke func _ input = -- split args from string into words before executing
        return $ CommandResult (func (words input)) (return (Exited ExitSuccess))

-- |Fork and exec a command with associated arguments. Return child PID.
-- Take a list of file descriptors needing to be closed and a string of
-- input to that command.
instance CommandLike (String, [String]) where
    invoke (cmd, args) closefds input =
      do (stdinread, stdinwrite) <- createPipe
         (stdoutread, stdoutwrite) <- createPipe

         -- Add parent FDs to close list because they must always be closed
         -- in the children.
         addCloseFDs closefds [stdinwrite, stdoutread]

         -- Fork the child, decide whether it should be run in background.
         childPID <- withMVar closefds (\fds -> forkProcess (child fds stdinread stdoutwrite))

         -- Close client-side FDs in parent.
         closeFd stdinread
         closeFd stdoutwrite

         -- Write the input to the command.
         stdinhdl <- fdToHandle stdinwrite
         forkIO $ do hPutStr stdinhdl input
                     hClose stdinhdl

         -- Prepare to receive output from the command.
         stdouthdl <- fdToHandle stdoutread

         return CommandResult {cmdOutput = hGetContents stdouthdl,
                              getExitStatus = wait childPID closefds stdinwrite stdoutread}

        where child closefds stdinread stdoutwrite =
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

                   executeFile cmd True args Nothing

-- |Wait on child PID if command is not being run in the background.
-- If backgrounded, spawn another thread in which waiting on pipes can be
-- done.
wait :: ProcessID -> CloseFDs -> Fd -> Fd -> IO ProcessStatus
wait childPID closefds stdinwrite stdoutread =
    do -- wait for child
       status <- getProcessStatus True False childPID
       -- unpack status; fail if status not present
       case status of
         Nothing -> fail "Error: Nothing from getProcessStatus"
         Just ps -> do removeCloseFDs closefds [stdinwrite, stdoutread]
                       return ps

-- |Evaluate two exit codes in a pipe and return a "combined" exit code.
-- Reflects the first error encountered.
getEC :: CommandResult -> IO CommandResult -> IO ProcessStatus
getEC src res2 =
    do sec <- getExitStatus src
       dest <- res2
       dec <- getExitStatus dest
       case sec of
         Exited ExitSuccess -> return dec
         x -> return x

-- |Parse an input string into a recursive Pipeline data structure.
pipeParser :: String -> Pipeline String
pipeParser str = toTree $ splitOn "|" $ unwords . words $ str :: Pipeline String
    where toTree cmds = case cmds of
                          [] -> Cmd "" -- invalid case
                          [x] -> if ">" `isInfixOf` x
                                   then let y = map (unwords . words) (splitOn ">" x)
                                        in Pipe (toTree [head y]) (HFile (last y))
                                   else Cmd (unwords . words $ x)
                          (x:xs) -> Pipe (toTree [x]) (toTree xs)

-- |Check if command should be backgrounded or not.
-- Return str unchanged if no & found; return without & if & found.
backgroundParser :: String -> (String, Background)
backgroundParser str = let cleanStr = unwords . words $ str
                       in case last cleanStr of
                            '&' -> (init cleanStr, True)
                            _ -> (str, False)

-- |Execute a 'CommandLike'.
runIO :: CommandLike a => a -> Bool -> IO()
runIO cmd background =
    do closefds <- newMVar [] -- init closefds list
       res <- invoke cmd closefds [] -- invoke the command

       -- Process output.
       output <- cmdOutput res
       putStr output

       -- Wait for termination and get exit status.
       let waitForExit = do ec <- getExitStatus res
                            case ec of
                              Exited ExitSuccess -> return ()
                              x -> do putStrLn "Uh oh, looks like that didn't work."
                                      return ()
       unless background waitForExit
       return ()
                
       
-- |Execute a command after it has been readline'd by prompt.
runCommand :: String -> IO ()
runCommand "" = prompt -- Nothing was entered, so return to prompt.
runCommand line = do
    addHistory line -- add this line to the command history
    env <- getEnvironment
    home <- getHomeDirectory

    -- Check if background command was sent.
    let (cmd, background) = backgroundParser line

    -- Replace environment variables and tilde, then form pipeline.
    let pipeline = pipeParser $ replaceEnvVars env $ replaceTilde home cmd

    -- Run the CommandLike Pipeline.
    runIO pipeline background

    -- Return to prompt.
    prompt

-- |Replace any instances of environment variables with their expanded
-- form.
replaceEnvVars :: [(String, String)] -> String -> String
replaceEnvVars env x =
    foldl (\acc xs ->
          if ("$" ++ fst xs) `isInfixOf` acc
            then replace ("$" ++ fst xs) (snd xs) acc
            else acc) x env

replaceTilde :: String -> String -> String
replaceTilde home = replace "~" ("/" ++ home)

-- |Add FDs to list of FDs that must be closed in a child after a fork.
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

-- |Remove FDs from the list of FDs that must be closed.
removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds toRemove =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist toRemove)

    where procfdlist = foldl removefd -- Remove FDs in fdlist from procfdlist
          -- Remove only the first occurrence of any given fd.
          removefd [] _ = []
          removefd (x:xs) fd
                | fd == x = xs -- fd found, remove and return
                -- fd not found, continue looking
                | otherwise = x : removefd xs fd

-- |Change directories.
hashCd :: [String] -> IO String
hashCd [arg] =
    do change <- tryIOError (changeWorkingDirectory arg)
       case change of
         Left _ -> return $ "cd: no such file or directory: " ++ arg
         Right _ -> return ""
hashCd _ = return "Not a valid path in pwd."

-- |Get help.
hashHelp :: [String] -> IO String
hashHelp _ =
    do let str = "------------------------ \n\
            \HASH -- Haskell, A SHell \n\
            \Author: Aaron Smith \n\
            \------------------------\n\n\
            \Builtins: \n"
           builtins = foldl (\x y -> x ++ "\n" ++ fst y) "" builtinCmds
       return $ str ++ builtins ++ "\n"

-- |Exit the shell. TODO: Fix exit so it exits.
hashExit :: [String] -> IO String
hashExit _ = do putStrLn "Exiting..."
                exitSuccess
                return ""

-- |Provide our own version of printenv that prints our own environment
-- variables.
hashPrintenv :: [String] -> IO String
hashPrintenv [] = do env <- getEnvironment
                     return $ foldl (\x y -> x ++ "\n" ++ fst y ++ "=" ++ snd y) "" env ++ "\n"
hashPrintenv _ = return "Too many arguments passed to printenv."

-- |Export an environment variable.
hashExport :: [String] -> IO String
hashExport [arg] = do let (x:y:_) = splitOn "=" arg
                      setEnv x y
                      return ""
hashExport _ = return "Wrong number of arguments passed to export."

-- |Set emacs or vi keybindings mode.
hashBindkey :: [String] -> IO String
hashBindkey [arg]
    | arg == "-v" = do keymap <- getKeymapByName "vi"
                       setKeymap keymap
                       return ""
    | arg == "-e" = do keymap <- getKeymapByName "emacs"
                       setKeymap keymap
                       return ""
hashBindkey _ = return "Wrong number of arguments passed to export."
