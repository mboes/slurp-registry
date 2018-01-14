{-# LANGUAGE OverloadedStrings #-}

module Slurp.Registry.Repository
  ( Repository(..)
  , clone
  , checkin
  , checkout
  , sync
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ((<>))
import qualified Path
import Path (Abs, Dir, File, Path)
import System.Exit (ExitCode(..))
import System.Process.Typed (proc, runProcess, runProcess_, setWorkingDir)

data Repository = Repository
  { repoPath :: Path Abs Dir
    -- | Lock to ensure that only one thread may write to the repository at
    -- any given time.
  , repoWriteLock :: MVar ()
  }

-- | Take the write lock on the repo
withRepoLock :: Repository -> (Path Abs Dir -> IO a) -> IO a
withRepoLock (Repository path lock) f = withMVar lock $ \() -> f path

clone :: MonadIO m => String -> Path Abs Dir -> m Repository
clone url cacheDir = liftIO $ do
    runProcess_ $ proc "git" ["clone", url, Path.toFilePath cacheDir]
    lock <- newMVar ()
    return $ Repository cacheDir lock

checkin :: MonadIO m => Repository -> Path Abs File -> BSL.ByteString -> m ()
checkin r packageFile content = liftIO $ withRepoLock r $ \path -> do
    BSL.writeFile (Path.toFilePath packageFile) content
    runProcess_ $
      setWorkingDir (Path.toFilePath path) $
      proc "git" ["add", Path.toFilePath packageFile]
    runProcess_ $
      setWorkingDir (Path.toFilePath path) $
      proc
        "git"
        [ "commit", "-m", "Add package " <> show packageFile]

checkout :: MonadIO m => Repository -> Path Abs File -> m (Maybe BSL.ByteString)
checkout Repository{repoPath = path} packageFile = liftIO $ do
    -- Always checkout first to avoid reading partial content from files.
    rc <- runProcess $
      setWorkingDir (Path.toFilePath path) $
      proc "git" ["checkout", Path.toFilePath packageFile]
    case rc of
      ExitSuccess -> Just <$> BSL.readFile (Path.toFilePath packageFile)
      ExitFailure _ -> return Nothing

-- | Sync with upstream master branch.
sync :: MonadIO m => Repository -> m ()
sync r = liftIO $ withRepoLock r $ \repo -> do
    runProcess_ $
      setWorkingDir (Path.toFilePath repo) $
      "git pull origin master"
