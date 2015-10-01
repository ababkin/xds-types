{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Xds.Directive.Types where

import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Vector (Vector)
import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson          (FromJSON (parseJSON), ToJSON (toJSON),
                                      Value (..), encode, object, (.:), (.=))
import           Data.Aeson.Types    (typeMismatch)

import Xds.Types (Id, URL, Path, Error)
import Xds.Stats.Types (Stats)


data Directive = Directive {
    datasetId :: Id
  , params :: [Param]
  , results :: [Result]
} deriving (Show, Generic)
instance FromJSON Directive
instance ToJSON Directive

data Param = DownloadParams {
    remoteUrl :: URL
  , s3Bucket  :: URL
  , s3Path    :: Path
  } deriving (Show, Generic)
instance FromJSON Param
instance ToJSON Param

data Result = DownloadSuccess {
    downloadedUrl :: URL
  } | DownloadFail Error 
    | ProcessSuccess {
    stats :: Stats 
  } | ProcessFail Error
  deriving (Show, Generic)
instance FromJSON Result
instance ToJSON Result





