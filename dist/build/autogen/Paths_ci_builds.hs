module Paths_ci_builds (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jamesvanneman/.cabal/bin"
libdir     = "/Users/jamesvanneman/.cabal/lib/x86_64-osx-ghc-7.8.4/ci-builds-0.1.0.0"
datadir    = "/Users/jamesvanneman/.cabal/share/x86_64-osx-ghc-7.8.4/ci-builds-0.1.0.0"
libexecdir = "/Users/jamesvanneman/.cabal/libexec"
sysconfdir = "/Users/jamesvanneman/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ci_builds_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ci_builds_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ci_builds_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ci_builds_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ci_builds_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
