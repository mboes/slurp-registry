{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Slurp.Registry.Handlers (Repository(..), server) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Path
import qualified Path.IO as Path
import qualified Servant
import Servant ((:<|>)(..))
import Slurp.Registry.API
import qualified Slurp.Registry.Repository as Repository
import Slurp.Registry.Repository (Repository(..))

-- | Add a package. This will:
--   - Sync the repository
--   - Load the correct file
--   - Insert the new record
--   - Rewrite the file
--   - Do a git commit
--   - Push to upstream
addPackage :: Repository -> Package -> IO AddPackageResponse
addPackage repo newpkg = do
    Repository.sync repo
    pkgFile <- Path.parseRelFile (Text.unpack (name newpkg))
    exists <- Path.doesFileExist pkgFile
    if exists
    then do
      Aeson.eitherDecodeStrict' <$> BS.readFile (Path.toFilePath pkgFile) >>= \case
        Left err -> fail err
        Right oldpkg
          | location oldpkg == location newpkg -> return PackageAdded
          | otherwise -> return $ PackageAlreadyOwned oldpkg
    else do
      Repository.commit repo (name newpkg) pkgFile (Aeson.encode newpkg)
      Repository.sync repo
      return PackageAdded

-- | List all packages
listPackages :: Repository -> IO [Package]
listPackages repo = do
    (_, files) <- Path.listDir $ repoPath repo
    packages <- forM files $ \file -> do
      either (\(_:: SomeException) -> Nothing) Aeson.decodeStrict' <$>
        try (BS.readFile $ Path.toFilePath file)
    return $ catMaybes packages

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
