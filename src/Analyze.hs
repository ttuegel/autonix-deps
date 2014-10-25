{-# LANGUAGE FlexibleContexts #-}

module Analyze
       ( analyzeFiles
       , analyzePackages
       , addBuildInput
       , addNativeBuildInput
       , matchFileName
       ) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.State (MonadState(..), StateT, execStateT, modify)
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Set as S
import Data.Traversable
import Prelude hiding (mapM)
import System.FilePath (takeFileName)

import Archive
import Manifest
import Types

analyzePackages :: MonadIO m => (ByteString -> FilePath -> m Deps)
                -> m [(ByteString, Deps)]
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
        let listeners = mapMaybe ($ file) analyzers
        unless (null listeners) $ do
            contents <- liftIO $ archiveView srcPath file
            mapM_ ($ contents) listeners
    return deps

matchFileName :: FilePath -> a -> FilePath -> Maybe a
matchFileName name a path
    | takeFileName path == name = Just a
    | otherwise = Nothing

addBuildInput :: MonadState Deps m => ByteString -> m ()
addBuildInput input = modify $ \deps ->
    if input `S.member` buildInputs deps
       then deps
    else deps { buildInputs = S.insert input $ buildInputs deps }

addNativeBuildInput :: MonadState Deps m => ByteString -> m ()
addNativeBuildInput input = modify $ \deps ->
    if input `S.member` nativeBuildInputs deps
       then deps
    else deps { nativeBuildInputs = S.insert input $ nativeBuildInputs deps }
