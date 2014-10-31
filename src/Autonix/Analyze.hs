{-# LANGUAGE FlexibleContexts #-}

module Autonix.Analyze
       ( Analyzer
       , analyzeFiles
       , analyzePackages
       , matchFileName
       ) where

import Control.Lens hiding (act)
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
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
    let (pkgs, _) = unzip manifest
    mapM_ (uncurry perPackage) manifest
    deps %= M.filterWithKey (\k _ -> S.member k $ S.fromList pkgs)

analyzeFiles :: MonadIO m => [Analyzer m] -> ByteString -> FilePath -> m ()
analyzeFiles analyzers pkg srcPath = do
    files <- archiveList srcPath
    forM_ files $ \file -> do
        let getFile = archiveView srcPath file
        mapM_ (\act -> act pkg file getFile) analyzers

matchFileName :: Monad m => FilePath -> (IO ByteString -> m ()) -> FilePath
              -> IO ByteString -> m ()
matchFileName name act path = when (takeFileName path == name) . act
