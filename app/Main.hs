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

import Control.Concurrent.MVar (newMVar)
import Control.Lens (makeLenses, non, to, (^.))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Handlers
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Options.Generic as Options
import Options.Generic ((:::), type (<?>))
import Path (parseAbsDir, toFilePath)
import Path.IO (createTempDir)
import qualified Servant
import Slurp.Registry.API
import System.IO (stderr)
import System.Process.Typed (proc, runProcess_)

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

-- | Initialise a local clone of the authoritative repository
initRepository :: RuntimeOptions Options.Unwrapped -> IO Repository
initRepository ro =
    case ro^.repositoryCacheDir of
      Nothing ->
        Path.withSystemTempDir "slurp" (clone (ro^.repositoryUrl))
      Just fpath -> do
        path <- Path.parseAbsDir fpath
        Path.withTempDir path "slurp" (clone (ro^.repositoryUrl))

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
