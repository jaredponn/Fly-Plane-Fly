{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_flyplanefly (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/bin"
libdir     = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/lib/x86_64-linux-ghc-8.2.2/flyplanefly-0.1.0.0-qnWfvLWevQSxOfngS5SJ-game"
dynlibdir  = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/share/x86_64-linux-ghc-8.2.2/flyplanefly-0.1.0.0"
libexecdir = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/libexec/x86_64-linux-ghc-8.2.2/flyplanefly-0.1.0.0"
sysconfdir = "/home/jared/Programs/Fly-Plane-Fly/.stack-work/install/x86_64-linux-ncurses6/lts-11.8/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "flyplanefly_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "flyplanefly_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "flyplanefly_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "flyplanefly_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "flyplanefly_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "flyplanefly_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
