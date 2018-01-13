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

import Control.Lens (makeLenses, non, (^.))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Logger as Wai
import Options.Generic ((:::), type (<?>))
import qualified Options.Generic as Options
import qualified Path
import qualified Path.IO as Path
import qualified Servant
import Slurp.Registry.API
import Slurp.Registry.Handlers
import qualified Slurp.Registry.Repository as Repository
import System.IO (stderr)

data RuntimeOptions w = RuntimeOptions
  { _serverPort :: w ::: Maybe Int
    <?> "Port on which to host the server."
  , _repositoryUrl :: w ::: String
    <?> "Authoritative server hosting the SLURP repository."
  , _repositoryCacheDir :: w ::: Maybe FilePath
    <?> "Where to cache the git repo locally."
  , _tlsCertificate :: w ::: Maybe Text
    <?> "Path to the TLS certificate to use when serving over HTTPS."
  , _tlsKey :: w ::: Maybe Text
    <?> "Path to the TLS key to use when serving over HTTPS."
  } deriving (Generic)
makeLenses ''RuntimeOptions

instance Options.ParseRecord (RuntimeOptions Options.Wrapped) where
  parseRecord = Options.parseRecordWithModifiers Options.lispCaseModifiers

deriving instance Show (RuntimeOptions Options.Unwrapped)

-- | Initialise a local clone of the authoritative repository.
initRepository :: RuntimeOptions Options.Unwrapped -> IO Repository
initRepository ro = do
    path <- maybe Path.getTempDir Path.parseAbsDir $ ro^.repositoryCacheDir
    Path.createTempDir path "slurp" >>= Repository.clone (ro^.repositoryUrl)

main :: IO ()
main = Wai.withStdoutLogger $ \logger -> do
    args <- Options.unwrapRecord "slurp-registry"
    repo <- initRepository args
    let settings =
          Warp.setPort (args^.serverPort.non 8080) $
          Warp.setLogger logger $
          Warp.defaultSettings
    case (args ^. tlsCertificate, args ^. tlsKey) of
      (Just cert, Just key) -> do
        Warp.runTLS
          (Warp.tlsSettings (Text.unpack cert) (Text.unpack key))
          settings
          (Servant.serve packageAPI (server repo))
      (Nothing, Nothing) ->
        Warp.runSettings settings (Servant.serve packageAPI (server repo))
      _ -> do
        Text.hPutStrLn
          stderr
          "Both --tls-key and --tls-certificate must be specified to enable HTTPS."
