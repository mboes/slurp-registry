{-# LANGUAGE OverloadedStrings #-}

module Slurp.Registry.Repository
  ( Repository(..)
  , clone
  , commit
  , sync
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Path
import Path (Abs, Dir, Path)
import System.Process.Typed (proc, runProcess_, setWorkingDir)

data Repository = Repository
  { repoPath :: Path Abs Dir
    -- | Lock to ensure that only one thread may write to the repository at
    -- any given time.
  , repoWriteLock :: MVar ()
  }

-- | Take the write lock on the repo
withRepoLock :: Repository -> (Path Abs Dir -> IO a) -> IO a
withRepoLock (Repository path lock) f = withMVar lock $ \() -> f path

clone :: String -> Path Abs Dir -> IO Repository
clone url cacheDir = do
    runProcess_ $ proc "git" ["clone", url, Path.toFilePath cacheDir]
    lock <- newMVar ()
    return $ Repository cacheDir lock

commit :: Repository -> Text.Text -> FilePath -> BSL.ByteString -> IO ()
commit r pkgname packageFile content = withRepoLock r $ \path -> do
    BSL.writeFile packageFile content
    runProcess_ $
      setWorkingDir (Path.toFilePath path) $
      proc "git" ["add", packageFile]
    runProcess_ $
      setWorkingDir (Path.toFilePath path) $
      proc
        "git"
        [ "commit", "-m", "Add package " <> Text.unpack pkgname, "."]

-- | Sync with upstream master branch.
sync :: Repository -> IO ()
sync r = withRepoLock r $ \repo -> do
    runProcess_ $
      setWorkingDir (Path.toFilePath repo) $
      "git pull origin master"
