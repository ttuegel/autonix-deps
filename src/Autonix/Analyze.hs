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

type Analyzer m = Text -> Sink (FilePath, ByteString) m ()

analyzePackages :: (MonadIO m, MonadState Deps m)
                => (Manifest -> m a)
                -> FilePath -> m ()
analyzePackages perPackage manifestPath = do
    manifest <- readManifest manifestPath
    let pkgs = map manifest_name manifest
    forM_ pkgs $ \pkg -> at pkg .= Just mempty
    mapM_ perPackage manifest
    deps %= M.filterWithKey (\k _ -> S.member k $ S.fromList pkgs)

sequenceSinks_ :: (Traversable f, Monad m) => f (Sink i m ()) -> Sink i m ()
sequenceSinks_ = void . sequenceSinks

analyzeFiles :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer (ResourceT m)] -> Manifest -> m ()
analyzeFiles analyzers manifest
    | null src = error $ T.unpack $ "No store path specified for " <> pkg
    | otherwise = do
        liftIO $ T.putStrLn $ "package " <> pkg
        runResourceT
            $ sourceArchive src $$ sequenceSinks_ (map ($ pkg) analyzers)
  where
    pkg = manifest_name manifest
    src = manifest_store manifest
