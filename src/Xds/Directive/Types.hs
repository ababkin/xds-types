{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-}

module Xds.Directive.Types where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
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
instance FromJSON Param where
  parseJSON o@(Object v) = do
    (t :: Text) <- v .: "type"
    case t of
      "download" -> DownloadParams
        <$> v .: "remoteUrl"
        <*> v .: "s3Bucket"
        <*> v .: "s3Path"
      _ -> 
        typeMismatch ("unexpected params type: " ++ T.unpack t) o
instance ToJSON Param where
  toJSON (DownloadParams {..})  =
    object  [
              "type"      .= ("download" :: Text)
            , "remoteUrl" .= remoteUrl
            , "s3Bucket"  .= s3Bucket
            , "s3Path"    .= s3Path
            ]
  toJSON params = error $ "could not serialize params to json: " ++ show params

data Result = DownloadSuccess {
    downloadedUrl :: URL
  } | DownloadFailure Error 
    | ProcessSuccess {
    stats :: Stats 
  } | ProcessFailure Error
  deriving (Show, Generic)
instance FromJSON Result where
  parseJSON o@(Object v) = do
    (t :: Text) <- v .: "type"
    (s :: Text) <- v .: "status"
    case (t,s) of
      ("download", "success") -> DownloadSuccess
        <$> v .: "downloadedUrl"
      ("download", "failure") -> DownloadFailure
        <$> v .: "error"
      ("process", "success") -> ProcessSuccess
        <$> v .: "stats"
      ("process", "failure") -> ProcessFailure
        <$> v .: "error"
      _ -> 
        typeMismatch ("unexpected type: " ++ T.unpack t ++ 
                      " and status: " ++ T.unpack s) o
instance ToJSON Result where
  toJSON (DownloadSuccess {..})  =
    object  [
              "type"          .= ("download" :: Text)
            , "status"        .= ("success" :: Text)
            , "downloadedUrl" .= downloadedUrl
            ]
  toJSON (DownloadFailure err)  =
    object  [
              "type"          .= ("download" :: Text)
            , "status"        .= ("failure" :: Text)
            , "error"         .= err 
            ]

  toJSON result = error $ "could not serialize result to json: " ++ show result 

