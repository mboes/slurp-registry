{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Lens (makeLenses, non, to, (^.))
import Control.Monad (forM, join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Options.Generic as Options
import Options.Generic ((:::), type (<?>))
import Path hiding ((</>))
import Path.IO (createTempDir, listDir)
import qualified Servant
import Servant ((:<|>)(..))
import Slurp.Registry.API
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process.Typed (proc, runProcess, runProcess_, setWorkingDir)

data RuntimeOptions w = RuntimeOptions
  { _serverPort :: w ::: Maybe Int <?> "Port on which to host the server."
  , _repositoryUrl :: w ::: Text <?> "Authoritative server hosting the SLURP repository"
  , _repositoryCacheDir :: w ::: Text <?> "Where to cache the git repo locally"
  , _tlsCertificate :: w ::: Maybe Text <?> "Path to the TLS certificate to use when serving HTTPS"
  , _tlsKey :: w ::: Maybe Text <?> "Path to the TLS key to use when serving HTTPS"
  } deriving (Generic)
makeLenses ''RuntimeOptions

instance Options.ParseRecord (RuntimeOptions Options.Wrapped) where
  parseRecord = Options.parseRecordWithModifiers Options.lispCaseModifiers

deriving instance Show (RuntimeOptions Options.Unwrapped)

data Repository = Repository
  { repoPath :: Path Abs Dir
    -- | Lock to ensure that only one thread may write to the repo at a given time
  , repoWriteLock :: MVar ()
  }

-- | Take the write lock on the repo
withRepoLock :: Repository -> (Path Abs Dir -> IO a) -> IO a
withRepoLock (Repository path lock) f = withMVar lock $ \() -> f path

-- | Initialise a local clone of the authoritative repository
initRepository :: RuntimeOptions Options.Unwrapped -> IO Repository
initRepository ro = do
    rootDir <- parseAbsDir $ ro^.repositoryCacheDir.to Text.unpack
    cacheDir <- createTempDir rootDir "slurp"
    runProcess_ $
      proc
        "git"
        ["clone", ro^.repositoryUrl.to Text.unpack, toFilePath cacheDir]
    lock <- newMVar ()
    return $ Repository cacheDir lock

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
              , "Add package " <> (Text.unpack $ name package)
              , "."
              ]
          runProcess_ $
            setWorkingDir (toFilePath path) $
            "git push origin master"
          return $ PackageAdded
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


main :: IO ()
main = do
    args <- Options.unwrapRecord "slurp-registry"
    repo <- initRepository args
    case (args ^. tlsCertificate, args ^. tlsKey) of
      (Just cert, Just key) -> do
        Warp.runTLS
          (Warp.tlsSettings (Text.unpack cert) (Text.unpack key))
          (Warp.setPort (args^.serverPort.non 8081) Warp.defaultSettings)
          (Servant.serve packageAPI $ server repo)
      (Nothing, Nothing) ->
        Warp.run (args^.serverPort.non 8081) (Servant.serve packageAPI $ server repo)
      _ -> do
        Text.hPutStrLn stderr "Both --tls-key and --tls-certificate must be\
                              \ specified to run HTTPS"
