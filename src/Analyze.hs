{-# LANGUAGE FlexibleContexts #-}

module Analyze
       ( analyzeFiles
       , addBuildInput
       , addNativeBuildInput
       , matchFileName
       ) where

import Control.Error
import Control.Monad (unless)
import Control.Monad.State (MonadState(..), StateT, execStateT, modify)
import Data.Monoid
import qualified Data.Set as S
import Data.Traversable
import Prelude hiding (mapM)
import System.FilePath (takeFileName)

import Archive
import Types

analyzeFiles :: [Analyzer (StateT Deps IO)] -> Manifest
             -> IO [(ByteString, Deps)]
analyzeFiles analyzers = mapM $ \(name, srcPath) -> do
    files <- archiveList srcPath
    deps <- flip execStateT mempty $ forM files $ \file -> do
        let listeners = mapMaybe ($ file) analyzers
        unless (null listeners) $ do
            contents <- archiveView srcPath file
            mapM_ ($ contents) listeners
    return (name, deps)

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
