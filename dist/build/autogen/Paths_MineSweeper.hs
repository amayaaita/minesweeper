module Paths_MineSweeper (
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

bindir     = "/home/shane/.cabal/bin"
libdir     = "/home/shane/.cabal/lib/x86_64-linux-ghc-7.10.3/MineSweeper-0.1.0.0-4XWHxiTbCKo6mzK9APUBrQ"
datadir    = "/home/shane/.cabal/share/x86_64-linux-ghc-7.10.3/MineSweeper-0.1.0.0"
libexecdir = "/home/shane/.cabal/libexec"
sysconfdir = "/home/shane/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MineSweeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MineSweeper_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MineSweeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MineSweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MineSweeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
