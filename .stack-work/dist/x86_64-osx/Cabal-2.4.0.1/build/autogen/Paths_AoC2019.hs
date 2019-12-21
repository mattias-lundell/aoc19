{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_AoC2019 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/bin"
libdir     = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/lib/x86_64-osx-ghc-8.6.5/AoC2019-0.0.0-LHwDqb9BY85HZiqrMRAAJh"
dynlibdir  = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/share/x86_64-osx-ghc-8.6.5/AoC2019-0.0.0"
libexecdir = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/libexec/x86_64-osx-ghc-8.6.5/AoC2019-0.0.0"
sysconfdir = "/Users/mattias/github/AoC2019/.stack-work/install/x86_64-osx/lts-14.16/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AoC2019_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AoC2019_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AoC2019_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AoC2019_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AoC2019_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AoC2019_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
