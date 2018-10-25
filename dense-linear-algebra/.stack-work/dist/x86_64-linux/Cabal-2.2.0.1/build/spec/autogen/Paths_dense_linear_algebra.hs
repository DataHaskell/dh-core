{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_dense_linear_algebra (
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

bindir     = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/bin"
libdir     = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/lib/x86_64-linux-ghc-8.4.3/dense-linear-algebra-0.1.0.0-28BiJq4beko48YOVpt6TFT-spec"
dynlibdir  = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/share/x86_64-linux-ghc-8.4.3/dense-linear-algebra-0.1.0.0"
libexecdir = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/libexec/x86_64-linux-ghc-8.4.3/dense-linear-algebra-0.1.0.0"
sysconfdir = "/home/atreyee/Projects/dh-core/dense-linear-algebra/.stack-work/install/x86_64-linux/lts-12.14/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dense_linear_algebra_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dense_linear_algebra_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "dense_linear_algebra_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "dense_linear_algebra_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dense_linear_algebra_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dense_linear_algebra_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
