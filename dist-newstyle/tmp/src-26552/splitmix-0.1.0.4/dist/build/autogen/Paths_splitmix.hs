{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_splitmix (
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
version = Version [0,1,0,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.2.4\\splitmix-0.1.0.4-88c5d46ff668576aaad6ee2e491b996af1f21a53\\etc"

getBinDir     = catchIO (getEnv "splitmix_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "splitmix_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "splitmix_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "splitmix_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "splitmix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "splitmix_sysconfdir") (\_ -> return sysconfdir)




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
