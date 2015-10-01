{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Xds.Directive.Util where

import Xds.Types (URL)
import Xds.Directive.Types (Directive(Directive, results), 
  Result(DownloadSuccess, downloadedUrl))


getDownloadedUrl :: Directive -> Maybe URL
getDownloadedUrl dir@Directive{ results = [] } = 
  Nothing
getDownloadedUrl Directive{ results = (DownloadSuccess{downloadedUrl}):_ } = 
  Just downloadedUrl
getDownloadedUrl dir@Directive{ results = _:rs } = 
  getDownloadedUrl dir{ results = rs }
