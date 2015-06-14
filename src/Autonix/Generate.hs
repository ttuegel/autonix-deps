{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix.Generate (generateDeps, writeRenames) where

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
             -> StateT Renames m ()
generateDeps analyzers = do
    pkgs <- withArgs $ analyzePackages (analyzeFiles analyzers)
    renames <- get
    writeRenames renames
    writePackages (applyRenames renames pkgs)

writeRenames :: MonadIO m => Renames -> m ()
writeRenames = liftIO . BL.writeFile "renames.json" . encodePretty

writePackages :: MonadIO m => Map Text Package -> m ()
writePackages = liftIO . BL.writeFile "packages.json" . encodePretty
