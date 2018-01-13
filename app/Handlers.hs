{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Handlers (Repository(..), server) where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM, join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Path (Path, Abs, Dir, toFilePath)
import Path.IO (listDir)
import qualified Servant
import Servant ((:<|>)(..))
import Slurp.Registry.API
import System.FilePath ((</>))
import System.Process.Typed (proc, runProcess_, setWorkingDir)

data Repository = Repository
  { repoPath :: Path Abs Dir
    -- | Lock to ensure that only one thread may write to the repo at a given time
  , repoWriteLock :: MVar ()
  }

-- | Take the write lock on the repo
withRepoLock :: Repository -> (Path Abs Dir -> IO a) -> IO a
withRepoLock (Repository path lock) f = withMVar lock $ \() -> f path

-- | Sync the clone with upstream master
syncRepository :: Repository -> IO ()
syncRepository r = withRepoLock r $ \repo -> do
    runProcess_ $
      setWorkingDir (toFilePath repo) $
      "git pull origin master"

-- | Add a package. This will:
--   - Sync the repository
--   - Load the correct file
--   - Insert the new record
--   - Rewrite the file
--   - Do a git commit
--   - Push to upstream
addPackage :: Repository -> Package -> IO AddPackageResponse
addPackage repo package = do
    syncRepository repo
    let indexChar = Text.index (name package) 0
        packageFile = toFilePath (repoPath repo) </> [indexChar]
    currentPackages <-
      either
        (\(_:: SomeException) -> [])
        (fromMaybe [] . Aeson.decodeStrict) <$>
        try (BS.readFile packageFile)
    case find (\p -> name p == name package) currentPackages of
      Nothing -> let newPackages = sort $ package : currentPackages in
        withRepoLock repo $ \path -> do
          BSL.writeFile packageFile $ Aeson.encode newPackages
          runProcess_ $
            setWorkingDir (toFilePath path) $
            proc "git" ["add", packageFile]
          runProcess_ $
            setWorkingDir (toFilePath path) $
            proc
              "git"
              [ "commit"
              , "-m"
              , "Add package " <> Text.unpack (name package)
              , "."
              ]
          runProcess_ $
            setWorkingDir (toFilePath path) $
            "git push origin master"
          return PackageAdded
      Just exists -> return $ PackageAlreadyOwned exists

-- | List all packages
listPackages :: Repository -> IO [Package]
listPackages repo = do
    (_, files) <- listDir $ repoPath repo
    packages <- forM files $ \file -> do
      either
        (\(_:: SomeException) -> [])
        (fromMaybe [] . Aeson.decodeStrict) <$>
        try (BS.readFile $ toFilePath file)
    return $ join packages

server :: Repository -> Servant.Server PackageAPI
server repo =
    listPackagesHandler :<|>
    addPackageHandler :<|>
    syncHandler
  where
    listPackagesHandler :: Servant.Handler [Package]
    listPackagesHandler = liftIO $ listPackages repo
    addPackageHandler :: Package -> Servant.Handler AddPackageResponse
    addPackageHandler = liftIO . addPackage repo
    syncHandler :: Servant.Handler Servant.NoContent
    syncHandler = liftIO (syncRepository repo) >> return Servant.NoContent
