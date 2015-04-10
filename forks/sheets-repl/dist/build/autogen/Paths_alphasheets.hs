module Paths_alphasheets (
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
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hal/code/alphasheets/.cabal-sandbox/bin"
libdir     = "/home/hal/code/alphasheets/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/alphasheets-0.0.0"
datadir    = "/home/hal/code/alphasheets/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/alphasheets-0.0.0"
libexecdir = "/home/hal/code/alphasheets/.cabal-sandbox/libexec"
sysconfdir = "/home/hal/code/alphasheets/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "alphasheets_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "alphasheets_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "alphasheets_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "alphasheets_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "alphasheets_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
