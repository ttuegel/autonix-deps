{-# LANGUAGE FlexibleContexts #-}

module Autonix.Analyze
       ( Analyzer
       , analyzeFiles
       , analyzePackages
       , matchFileName
       ) where

import Control.Lens hiding (act)
import Control.Monad.State
import Data.Monoid
import Prelude hiding (mapM)
import System.FilePath (takeFileName)

import Autonix.Archive
import Autonix.Deps
import Autonix.Manifest

type Analyzer m = ByteString -> FilePath -> IO ByteString -> m ()

analyzePackages :: (MonadIO m, MonadState Deps m)
                => (ByteString -> FilePath -> m a) -> m ()
analyzePackages perPackage = do
    manifest <- readManifest
    forM_ manifest $ \(pkg, _) -> at pkg .= Just mempty
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
