{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix (autonix) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import Options.Applicative

import Autonix.Analyze
import Autonix.Package (Package)
import Autonix.Renames

autonix :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
        => [Analyzer (StateT (Map Text Package, Renames) m)]
        -> m ()
autonix analyzers = do
    (pkgs, renames) <- withArgs (analyzePackages (analyzeFiles analyzers))
    writePackages pkgs
    writeRenames renames

withArgs :: MonadIO m => (FilePath -> m a) -> m a
withArgs child =
    join $ liftIO $ execParser $ info (helper <*> parser) fullDesc
  where
    parser = child <$> strArgument (metavar "MANIFEST")

writePackages :: MonadIO m => Map Text Package -> m ()
writePackages = liftIO . BL.writeFile "packages.json" . encodePretty
