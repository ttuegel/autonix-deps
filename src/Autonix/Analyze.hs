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
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude hiding (mapM)

import Autonix.Manifest (Manifest, readManifests)
import qualified Autonix.Manifest as Manifest
import Autonix.Package (Package, package)
import Autonix.Renames

type Analyzer m = Text -> Manifest -> Sink (FilePath, ByteString) (ResourceT m) ()

analyzePackages :: MonadIO m => (Text -> Manifest -> StateT (Map Text Package, Renames) m ())
                -> FilePath -> m (Map Text Package, Renames)
analyzePackages perPackage manifestPath = flip execStateT (mempty, mempty) $ do
    manifests <- readManifests manifestPath
    imapM_ perPackage manifests

sequenceSinks_ :: (Traversable f, Monad m) => f (Sink i m ()) -> Sink i m ()
sequenceSinks_ = void . sequenceSinks

analyzeFiles :: (MonadBaseControl IO m, MonadIO m, MonadState (Map Text Package, Renames) m, MonadThrow m)
             => [Analyzer m] -> Text -> Manifest -> m ()
analyzeFiles analyzers name m
    | null (m^.Manifest.store) = error (T.unpack noStore)
    | otherwise = do
        liftIO $ T.putStrLn $ "package " <> name
        let conduits = sourceArchive (m^.Manifest.store) $$ sequenceSinks_ sinks
            sinks = analyzers >>= \analyze -> return (analyze name m)
            pkg = package (m^.Manifest.name) (m^.Manifest.src)
        _1 . at name %= Just . maybe pkg (pkg <>)
        runResourceT conduits
  where
    noStore = "No store path specified for " <> (m^.Manifest.name)
