{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_epistemic_group_gossip (
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

bindir     = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/bin"
libdir     = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/lib/x86_64-linux-ghc-8.10.4/epistemic-group-gossip-0.1.0.0-GVEP0oEoH7LKlhgUTui3WB-egg"
dynlibdir  = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/share/x86_64-linux-ghc-8.10.4/epistemic-group-gossip-0.1.0.0"
libexecdir = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/libexec/x86_64-linux-ghc-8.10.4/epistemic-group-gossip-0.1.0.0"
sysconfdir = "/mnt/c/Users/Jesper/Documents/local-study/1-4-lamas/projects/epistemic-group-gossip/.stack-work/install/x86_64-linux-tinfo6/14ea3c5a9fdd93d69e041773da83757947d8425f7233a9a6f260ae39bcd34d9e/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "epistemic_group_gossip_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "epistemic_group_gossip_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "epistemic_group_gossip_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "epistemic_group_gossip_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "epistemic_group_gossip_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "epistemic_group_gossip_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
