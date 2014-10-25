{-# LANGUAGE FlexibleContexts #-}

module Analyze
       ( analyzeFiles
       , analyzePackages
       , matchFileName
       ) where

import Control.Monad.State
import Data.Monoid
import Prelude hiding (mapM)
import System.FilePath (takeFileName)

import Archive
import Manifest
import Types

analyzePackages :: MonadIO m => (ByteString -> FilePath -> m a)
                -> m [(ByteString, a)]
analyzePackages perPackage = do
    manifest <- readManifest
    deps <- mapM (uncurry perPackage) manifest
    let (names, _) = unzip manifest
    return $ zip names deps

analyzeFiles :: MonadIO m => [Analyzer (StateT Deps m)]
             -> ByteString -> FilePath -> m Deps
analyzeFiles analyzers _ srcPath = do
    files <- archiveList srcPath
    deps <- flip execStateT mempty $ forM files $ \file -> do
        let getFile = archiveView srcPath file
        mapM_ (\act -> act file getFile) analyzers
    return deps

matchFileName :: Monad m => FilePath -> (IO ByteString -> m ()) -> FilePath
              -> IO ByteString -> m ()
matchFileName name act path = when (takeFileName path == name) . act
