{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Slurp.Registry.Handlers (Repository(..), server) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Path
import Path.IO (listDir)
import qualified Servant
import Servant ((:<|>)(..))
import Slurp.Registry.API
import qualified Slurp.Registry.Repository as Repository
import Slurp.Registry.Repository (Repository(..))
import System.FilePath ((</>))

-- | Add a package. This will:
--   - Sync the repository
--   - Load the correct file
--   - Insert the new record
--   - Rewrite the file
--   - Do a git commit
--   - Push to upstream
addPackage :: Repository -> Package -> IO AddPackageResponse
addPackage repo package = do
    Repository.sync repo
    let indexChar = Text.index (name package) 0
        packageFile = Path.toFilePath (repoPath repo) </> [indexChar]
    currentPackages <-
      either
        (\(_:: SomeException) -> [])
        (fromMaybe [] . Aeson.decodeStrict) <$>
        try (BS.readFile packageFile)
    case find (\p -> name p == name package) currentPackages of
      Nothing -> do
        let newPackages = sort $ package : currentPackages
        Repository.commit repo (name package) packageFile (Aeson.encode newPackages)
        Repository.sync repo
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
        try (BS.readFile $ Path.toFilePath file)
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
    syncHandler = liftIO (Repository.sync repo) >> return Servant.NoContent
