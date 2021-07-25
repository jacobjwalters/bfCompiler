{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_bfCompiler (
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

bindir     = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/bin"
libdir     = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/lib/x86_64-linux-ghc-8.10.5/bfCompiler-0.1.0.0-L3eL8RLpRAeJxVu6L5rCIK-bfCompiler"
dynlibdir  = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/lib/x86_64-linux-ghc-8.10.5"
datadir    = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/share/x86_64-linux-ghc-8.10.5/bfCompiler-0.1.0.0"
libexecdir = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/libexec/x86_64-linux-ghc-8.10.5/bfCompiler-0.1.0.0"
sysconfdir = "/home/csm/docs/prog/hs/bfCompiler/.stack-work/install/x86_64-linux-tinfo6/55c31ca566bb0a80fea1cc7c9fd3d50e8939b4a73958a25d73a6f0215495259d/8.10.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bfCompiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bfCompiler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bfCompiler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bfCompiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bfCompiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bfCompiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
