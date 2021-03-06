module Paths_SelectionFunctions (
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

bindir     = "/home/stepi/.cabal/bin"
libdir     = "/home/stepi/.cabal/lib/x86_64-linux-ghc-7.6.3/SelectionFunctions-0.1.0.0"
datadir    = "/home/stepi/.cabal/share/x86_64-linux-ghc-7.6.3/SelectionFunctions-0.1.0.0"
libexecdir = "/home/stepi/.cabal/libexec"
sysconfdir = "/home/stepi/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SelectionFunctions_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SelectionFunctions_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SelectionFunctions_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SelectionFunctions_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SelectionFunctions_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
