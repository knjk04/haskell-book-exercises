{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hello (
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

bindir     = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/bin"
libdir     = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/lib/x86_64-linux-ghc-8.4.3/hello-0.1.0.0-LZymhyrj3r82ycDtU1nQWN-hello"
dynlibdir  = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/share/x86_64-linux-ghc-8.4.3/hello-0.1.0.0"
libexecdir = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/libexec/x86_64-linux-ghc-8.4.3/hello-0.1.0.0"
sysconfdir = "/home/karan/Documents/haskell-book-exercises/round2/haskell-book-exercises/src/ch13/practice/hello/.stack-work/install/x86_64-linux-tinfo6/lts-12.10/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hello_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hello_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
