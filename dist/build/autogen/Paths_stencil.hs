{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_stencil (
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

bindir     = "/home/isaac/.cabal/bin"
libdir     = "/home/isaac/.cabal/lib/x86_64-linux-ghc-8.0.2/stencil-0.1.0.0-GsjfoDpNa5TFoM0aM1gk9U"
dynlibdir  = "/home/isaac/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/isaac/.cabal/share/x86_64-linux-ghc-8.0.2/stencil-0.1.0.0"
libexecdir = "/home/isaac/.cabal/libexec/x86_64-linux-ghc-8.0.2/stencil-0.1.0.0"
sysconfdir = "/home/isaac/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stencil_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stencil_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "stencil_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "stencil_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stencil_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stencil_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
