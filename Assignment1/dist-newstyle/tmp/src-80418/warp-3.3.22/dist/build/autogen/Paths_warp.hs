{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_warp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,3,22] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\bin"
libdir     = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\lib"
datadir    = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\share"
libexecdir = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-8.6.5\\warp-3.3.22-75a092446a0b5bd4c0353e2f1bd343a5475c38da\\etc"

getBinDir     = catchIO (getEnv "warp_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "warp_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "warp_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "warp_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warp_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
