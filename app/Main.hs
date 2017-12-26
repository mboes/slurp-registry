{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Swagger
import System.Environment (getArgs)
import qualified Text.URI as URI

type PackageAPI
  = "packages" :> Get '[JSON] [Package] :<|>
    "package" :> Capture "name" Text :> Get '[JSON] Package

data Package = Package
  { name :: Text
  , location :: URI.URI
  , date :: Time.UTCTime
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON Package where
  toJSON Package{..} = Aeson.object
    [ "name" .= name
    , "location" .= URI.render location
    , "date" .=
        Time.formatTime
          Time.defaultTimeLocale
          (Time.iso8601DateFormat (Just "%H:%M:%SZ"))
          date
    ]

instance Aeson.FromJSON Package where
  parseJSON = Aeson.withObject "Package" $ \v -> Package
    <$> v .: "name"
    <*> (do
      txt <- v .: "location"
      case URI.mkURI txt of
        Left exc -> fail (show exc)
        Right uri -> return uri)
    <*> v .: "date"

packageAPI :: Proxy PackageAPI
packageAPI = Proxy

server :: Server PackageAPI
server = return [] :<|> return undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port, gitURL] -> Warp.run 8081 (serve packageAPI server)
    _ -> fail "Usage: slurp-registry <PORT> <GIT_URL>"
