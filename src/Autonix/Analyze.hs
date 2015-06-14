{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Autonix.Analyze
       ( Analyzer
       , analyzeFiles
       , analyzePackages
       ) where

import Codec.Archive
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (mapM)

import Autonix.Manifest
import Autonix.Package (Package, package)
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
    | null (manifest^.store) = error $ T.unpack noStore
    | otherwise = do
        liftIO $ T.putStrLn $ "package " <> (manifest^.name)
        let conduits = sourceArchive (manifest^.store)
                       $$ sequenceSinks_ (map ($ (manifest^.name)) analyzers)
            pkg = package (manifest^.name) (manifest^.src)
        execStateT (runResourceT conduits) pkg
  where
    noStore = "No store path specified for " <> (manifest^.name)
