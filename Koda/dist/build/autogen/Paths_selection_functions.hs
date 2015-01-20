module Paths_selection_functions (
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

bindir     = "\\\\Spin\\stepisnikt$\\_System\\ApplicationData\\cabal\\bin"
libdir     = "\\\\Spin\\stepisnikt$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\selection-functions-0.1.0.0"
datadir    = "\\\\Spin\\stepisnikt$\\_System\\ApplicationData\\cabal\\i386-windows-ghc-7.6.3\\selection-functions-0.1.0.0"
libexecdir = "\\\\Spin\\stepisnikt$\\_System\\ApplicationData\\cabal\\selection-functions-0.1.0.0"
sysconfdir = "\\\\Spin\\stepisnikt$\\_System\\ApplicationData\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "selection_functions_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "selection_functions_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "selection_functions_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "selection_functions_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "selection_functions_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
