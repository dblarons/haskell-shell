module Paths_hash (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/aaron/Library/Haskell/bin"
libdir     = "/Users/aaron/Library/Haskell/ghc-7.8.3-x86_64/lib/hash-0.1.0.0"
datadir    = "/Users/aaron/Library/Haskell/share/ghc-7.8.3-x86_64/hash-0.1.0.0"
libexecdir = "/Users/aaron/Library/Haskell/libexec"
sysconfdir = "/Users/aaron/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hash_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hash_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hash_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hash_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hash_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
