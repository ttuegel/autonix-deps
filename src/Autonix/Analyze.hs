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
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (mapM)

import Autonix.Manifest (Manifest, readManifests)
import qualified Autonix.Manifest as Manifest
import Autonix.Package (Package, package)
import Autonix.Renames

type Analyzer m = Text -> Sink (FilePath, ByteString) (ResourceT (StateT Package m)) ()

analyzePackages :: (MonadIO m, MonadState Renames m)
                => (Text -> Manifest -> m Package)
                -> FilePath -> m (Map Text Package)
analyzePackages perPackage manifestPath = do
    manifests <- readManifests manifestPath
    M.traverseWithKey perPackage manifests

sequenceSinks_ :: (Traversable f, Monad m) => f (Sink i m ()) -> Sink i m ()
sequenceSinks_ = void . sequenceSinks

analyzeFiles :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer m] -> Text -> Manifest -> m Package
analyzeFiles analyzers name m
    | null (m^.Manifest.store) = error (T.unpack noStore)
    | otherwise = do
        liftIO $ T.putStrLn $ "package " <> name
        let conduits = sourceArchive (m^.Manifest.store)
                       $$ sequenceSinks_ (map ($ name) analyzers)
            pkg = package (m^.Manifest.name) (m^.Manifest.src)
        execStateT (runResourceT conduits) pkg
  where
    noStore = "No store path specified for " <> (m^.Manifest.name)
