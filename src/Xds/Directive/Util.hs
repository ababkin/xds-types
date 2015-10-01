{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xds.Directive.Util where

import Xds.Types (URL)
import Xds.Directive.Types (Directive(Directive, params, results), 
  Param(DownloadParams),
  Result(DownloadSuccess, downloadedUrl))

getDownloadedParams :: Directive -> Maybe Param
getDownloadedParams dir@Directive{ params = [] } = 
  Nothing
getDownloadedParams Directive{ params = (dp@DownloadParams{}):_ } = 
  Just dp 
getDownloadedParams dir@Directive{ params = _:ps } = 
  getDownloadedParams dir{ params = ps }


getDownloadedUrl :: Directive -> Maybe URL
getDownloadedUrl dir@Directive{ results = [] } = 
  Nothing
getDownloadedUrl Directive{ results = (DownloadSuccess{downloadedUrl}):_ } = 
  Just downloadedUrl
getDownloadedUrl dir@Directive{ results = _:rs } = 
  getDownloadedUrl dir{ results = rs }
