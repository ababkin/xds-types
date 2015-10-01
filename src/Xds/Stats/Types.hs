{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Xds.Stats.Types where

import           Control.Applicative      ((<$>), (<*>))
import           Control.DeepSeq          (NFData (..))
import           Data.Aeson               (ToJSON (toJSON), FromJSON (parseJSON), Value(Object), object, (.=), (.:))
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Monoid              (mempty, mappend, Monoid ())
import           Data.Text                (Text)
import           GHC.Generics
import Data.Time.Clock (UTCTime)
{- import           Data.Vector.Unboxed      (Vector) -}
{- import qualified Data.Vector.Unboxed      as V -}
import           Data.Vector      (Vector)
import qualified Data.Vector      as V

data Stats = Stats {
    fields     :: !(Vector Field)
  , numRecords :: !Int
  } deriving Show
{- instance ToJSON Stats -}
instance FromJSON Stats where
  parseJSON (Object v) = Stats 
                       <$> v .: "fields"
                       <*> v .: "numRecords"

instance ToJSON Stats where
  toJSON (Stats fields numRecords)    =
    object  [
              "fields"     .= fields
            , "numRecords" .= numRecords
            ]

newtype Histogram a = Histogram {unHistogram :: Map a Int} deriving Show
instance Ord a => Monoid (Histogram a) where
  mempty = Histogram M.empty
  (Histogram !map1) `mappend` (Histogram !map2) =
    Histogram $! M.unionWith (+) map1 map2
    {- Histogram $! trim $ M.unionWith (+) map1 map2 -}
    {- where -}
      {- trim :: Ord a => Map a Int -> Map a Int -}
      {- trim mp = M.filterWithKey tf mp -}
        {- where -}
          {- tf _ a = a >= cutoff -}
          {- cutoff = go 10 vals -}
          {- [> go :: (V.Unbox a, Ord a) => Int -> Vector a -> a <] -}
          {- go :: (Ord a) => Int -> Vector a -> a -}
          {- go !n !es | V.length es == 1  = V.head es -}
                    {- | n < 2             = V.maximum es -}
                    {- | otherwise         =  -}
                        {- go (n-1) $! V.ifilter (\(!i) _ -> i /= V.maxIndex es) es -}
          {- vals = V.fromList . M.elems $ mp -}

instance ToJSON (Histogram Text) where
  toJSON (Histogram{unHistogram = m}) = toJSON m
instance FromJSON (Histogram Text) where
  parseJSON = fmap Histogram . parseJSON

instance ToJSON (Histogram UTCTime) where
  toJSON (Histogram{unHistogram = m}) = toJSON $ M.mapKeys show m
instance FromJSON (Histogram UTCTime) where
  parseJSON = fmap (Histogram . M.mapKeys read) . parseJSON

instance ToJSON (Histogram Double) where
  toJSON (Histogram{unHistogram = m}) = toJSON $ M.mapKeys show m
instance FromJSON (Histogram Double) where
  parseJSON = fmap (Histogram . M.mapKeys read) . parseJSON

data Field = EmptyField {
    fName :: !Text
  } | DoubleField {
    fName     :: !Text
  , fnMin     :: !Double
  , fnMax     :: !Double
  , fnMean    :: !Double
  , fnVar     :: !Double
  , fnHist    :: !(Histogram Double)
  , fNonEmpty :: !Int
  } | DateTimeField {
    fName     :: !Text
  , fdMin     :: !UTCTime
  , fdMax     :: !UTCTime
  , fdHist    :: !(Histogram UTCTime)
  , fNonEmpty :: !Int
  } | TextField {
    fName     :: !Text
  , ftHist    :: !(Histogram Text)
  , fNonEmpty :: !Int
  } deriving (Generic, Show)
instance ToJSON Field where
  toJSON (EmptyField {..})  =
    object  [
              "data_type" .= ("empty" :: Text)
            , "name"      .= fName
            ]
  toJSON (DoubleField {..}) =
    object  [
              "data_type" .= ("number" :: Text)
            , "name"      .= fName
            , "min"       .= fnMin
            , "max"       .= fnMax
            , "mean"      .= fnMean
            {- , "variance"  .= fnVar -}
            , "stddev"    .= sqrt fnVar
            , "hist"      .= fnHist
            , "non_empty" .= fNonEmpty
            ]
  toJSON (DateTimeField {..}) =
    object  [
              "data_type" .= ("datetime" :: Text)
            , "name"      .= fName
            , "min"       .= fdMin
            , "max"       .= fdMax
            , "hist"      .= fdHist
            , "non_empty" .= fNonEmpty
            ]
  toJSON (TextField {..})   =
    object  [
              "data_type" .= ("text" :: Text)
            , "name"      .= fName
            , "hist"      .= ftHist
            {- , "factors"   .= mp -}
            , "non_empty" .= fNonEmpty
            ]

instance FromJSON Field where
  parseJSON (Object v) = do
    (t :: Text) <- v .: "data_type"
    case t of
      "empty"   -> EmptyField <$> v .: "name"
      "number"  -> DoubleField 
                <$> v .: "name"
                <*> v .: "min"
                <*> v .: "max"
                <*> v .: "mean"
                <*> v .: "stddev"
                <*> v .: "hist"
                <*> v .: "non_empty"
      "datetime"  -> DateTimeField 
                <$> v .: "name"
                <*> v .: "min"
                <*> v .: "max"
                <*> v .: "hist"
                <*> v .: "non_empty"
      "datetime"  -> TextField 
                <$> v .: "name"
                <*> v .: "hist"
                <*> v .: "non_empty"

instance NFData Field


