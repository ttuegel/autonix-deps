{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Autonix.Analyze
       ( Analyzer
       , analyzeFiles
       , analyzePackages
       ) where

import Codec.Archive
import Control.Lens hiding (act)
import Control.Monad.State
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (mapM)

import Autonix.Deps
import Autonix.Manifest
import Autonix.Package
import Autonix.Renames

type Analyzer m = Text -> Sink (FilePath, ByteString) (ResourceT (StateT Package m)) ()

analyzePackages :: (MonadIO m, MonadState Renames m)
                => (Manifest -> m Package)
                -> FilePath -> m (Map Text Package)
analyzePackages perPackage manifestPath = do
    manifests <- readManifests manifestPath
    mapM perPackage manifests

sequenceSinks_ :: (Traversable f, Monad m) => f (Sink i m ()) -> Sink i m ()
sequenceSinks_ = void . sequenceSinks

analyzeFiles :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer m] -> Manifest -> m Package
analyzeFiles analyzers manifest
    | null src = error $ T.unpack $ "No store path specified for " <> pkg
    | otherwise = do
        liftIO $ T.putStrLn $ "package " <> pkg
        let conduits = sourceArchive src $$ sequenceSinks_ (map ($ pkg) analyzers)
        execStateT (runResourceT conduits) (package
                                            (manifest_name manifest)
                                            (manifest_src manifest))
  where
    pkg = manifest_name manifest
    src = manifest_store manifest
