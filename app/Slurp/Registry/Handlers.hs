{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Slurp.Registry.Handlers (Repository(..), server) where

import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Path
import qualified Path.IO as Path
import qualified Servant
import Servant ((:<|>)(..))
import Slurp.Registry.API
import qualified Slurp.Registry.Repository as Repository
import Slurp.Registry.Repository (Repository(..))

-- | Check that the package name is legal. Throw an error otherwise.
checkPackageName :: Text.Text -> Servant.Handler ()
checkPackageName pkgname
  | Text.all legalChar pkgname
  , Text.length pkgname >= 1
  , Text.length pkgname <= 64
  , Text.head pkgname /= '-'
  , Text.last pkgname /= '-'
  = return ()
  | otherwise
  = Servant.throwError Servant.err400 { Servant.errBody = "Invalid package name." }
  where
    legalChar c = isAlphaNum c || c == '-'

-- | Add a package. This will:
--   - Sync the repository
--   - Load the correct file
--   - Insert the new record
--   - Rewrite the file
--   - Do a git commit
--   - Push to upstream
addPackage :: Repository -> Package -> Servant.Handler Servant.NoContent
addPackage repo newpkg = do
    checkPackageName (name newpkg)
    liftIO $ Repository.sync repo
    pkgFile <-
      (repoPath repo Path.</>) <$>
      Path.parseRelFile (Text.unpack (name newpkg))
    exists <- Path.doesFileExist pkgFile
    if exists
    then do
      Aeson.eitherDecodeStrict' <$>
        liftIO (BS.readFile (Path.toFilePath pkgFile)) >>= \case
          Left err -> fail err
          Right oldpkg
            | location oldpkg == location newpkg -> return Servant.NoContent
            | otherwise -> Servant.throwError $
              Servant.err400 { Servant.errBody = "Package already added." }
    else do
      liftIO $ Repository.checkin repo pkgFile (Aeson.encode newpkg)
      liftIO $ Repository.sync repo
      return Servant.NoContent

-- | List all packages
listPackages :: Repository -> Servant.Handler [Package]
listPackages repo = liftIO $ do
    (_, files) <- Path.listDir $ repoPath repo
    packages <- forM files $ \file -> do
      either (\(_:: SomeException) -> Nothing) Aeson.decode' <$>
        try (Repository.checkout repo file)
    return $ catMaybes packages

server :: Repository -> Servant.Server PackageAPI
server repo =
    listPackagesHandler :<|>
    addPackageHandler :<|>
    syncHandler
  where
    listPackagesHandler :: Servant.Handler [Package]
    listPackagesHandler = listPackages repo
    addPackageHandler :: Package -> Servant.Handler Servant.NoContent
    addPackageHandler = addPackage repo
    syncHandler :: Servant.Handler Servant.NoContent
    syncHandler = liftIO (Repository.sync repo) >> return Servant.NoContent
