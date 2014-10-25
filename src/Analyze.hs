{-# LANGUAGE FlexibleContexts #-}

module Analyze
       ( Analyzer
       , analyzeFiles
       , analyzePackages
       , matchFileName
       ) where

import Control.Monad.State
import Data.ByteString (ByteString)
import Prelude hiding (mapM)
import System.FilePath (takeFileName)

import Archive
import Manifest

type Analyzer m = ByteString -> FilePath -> IO ByteString -> m ()

analyzePackages :: MonadIO m => (ByteString -> FilePath -> m a) -> m ()
analyzePackages perPackage = do
    manifest <- readManifest
    mapM_ (uncurry perPackage) manifest

analyzeFiles :: MonadIO m => [Analyzer m] -> ByteString -> FilePath -> m ()
analyzeFiles analyzers pkg srcPath = do
    files <- archiveList srcPath
    forM_ files $ \file -> do
        let getFile = archiveView srcPath file
        mapM_ (\act -> act pkg file getFile) analyzers

matchFileName :: Monad m => FilePath -> (IO ByteString -> m ()) -> FilePath
              -> IO ByteString -> m ()
matchFileName name act path = when (takeFileName path == name) . act
