{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_trans_service (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lorcan/distroFS/transaction-service/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-7.10/8.0.1/bin"
libdir     = "/home/lorcan/distroFS/transaction-service/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-7.10/8.0.1/lib/x86_64-linux-ghc-8.0.1/trans-service-0.1.0.0-H3gX8iFiR5C8lABfZwOo2"
datadir    = "/home/lorcan/distroFS/transaction-service/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-7.10/8.0.1/share/x86_64-linux-ghc-8.0.1/trans-service-0.1.0.0"
libexecdir = "/home/lorcan/distroFS/transaction-service/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-7.10/8.0.1/libexec"
sysconfdir = "/home/lorcan/distroFS/transaction-service/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-7.10/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trans_service_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trans_service_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "trans_service_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trans_service_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trans_service_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
