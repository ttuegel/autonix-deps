{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix.Generate (generateDeps) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)

import Autonix.Analyze
import Autonix.Args
import Autonix.Package (Package)
import Autonix.Renames

generateDeps :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer (StateT Renames m)]
             -> (Map Text Package -> Map Text Package)
             -> StateT Renames m ()
generateDeps analyzers post = do
    pkgs <- withArgs (analyzePackages (analyzeFiles analyzers))
    renames <- get
    writePackages (post (applyRenames renames pkgs))

writePackages :: MonadIO m => Map Text Package -> m ()
writePackages = liftIO . BL.writeFile "packages.json" . encodePretty
