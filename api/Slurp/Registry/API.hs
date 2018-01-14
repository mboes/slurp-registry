{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Slurp.Registry.API where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Time as Time
import GHC.Generics (Generic)
import Servant
import qualified Text.URI as URI
import Data.Ord (comparing)

data Package = Package
  { name     :: Text
  , location :: URI.URI
  , ctime     :: Maybe Time.UTCTime
  } deriving (Eq, Show, Generic)

instance Ord Package where
  compare = comparing name

instance Aeson.ToJSON Package where
  toJSON Package{..} = Aeson.object
      [ "name" .= name
      , "location" .= URI.render location
      , "ctime" .=
        (Time.formatTime
          Time.defaultTimeLocale
          (Time.iso8601DateFormat (Just "%H:%M:%SZ")) <$>
          ctime)
      ]

instance Aeson.FromJSON Package where
  parseJSON = Aeson.withObject "Package" $ \v -> Package
      <$> v .: "name"
      <*> (do
        txt <- v .: "location"
        case URI.mkURI txt of
          Left exc  -> fail (show exc)
          Right uri -> return uri)
      <*> v .:? "ctime"

data AddPackageResponse
  = PackageAdded
  | PackageAlreadyOwned Package
  | PackageNameInvalid
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON AddPackageResponse
instance Aeson.ToJSON AddPackageResponse

type PackageAPI
  = "packages" :> Get '[JSON] [Package]
  :<|> "packages" :> ReqBody '[JSON] Package :> Post '[JSON] AddPackageResponse
  :<|> "sync" :> Post '[JSON] NoContent

packageAPI :: Proxy PackageAPI
packageAPI = Proxy
