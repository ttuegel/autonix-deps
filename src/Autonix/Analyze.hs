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
import Prelude hiding (mapM)

import Autonix.Deps
import Autonix.Manifest

type Analyzer m = ByteString -> Sink (FilePath, ByteString) m ()

analyzePackages :: (MonadIO m, MonadState Deps m)
                => (ByteString -> FilePath -> m a)
                -> FilePath -> m ()
analyzePackages perPackage manifestPath = do
    manifest <- readManifest manifestPath
    forM_ manifest $ \(pkg, _) -> at pkg .= Just mempty
    let (pkgs, _) = unzip manifest
    mapM_ (uncurry perPackage) manifest
    deps %= M.filterWithKey (\k _ -> S.member k $ S.fromList pkgs)

sequenceSinks_ :: (Traversable f, Monad m) => f (Sink i m ()) -> Sink i m ()
sequenceSinks_ = void . sequenceSinks

analyzeFiles :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer (ResourceT m)] -> ByteString -> FilePath -> m ()
analyzeFiles analyzers pkg src
    | null src = error $ B.unpack $ "No store path specified for " <> pkg
    | otherwise = do
        liftIO $ B.putStrLn $ "package " <> pkg
        runResourceT
            $ sourceArchive src $$ sequenceSinks_ (map ($ pkg) analyzers)
