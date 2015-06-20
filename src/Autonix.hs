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

autonix :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
        => [Analyzer (StateT (Map Text Package) m)]
        -> (Map Text Package -> Map Text Package)
        -> m ()
autonix analyzers post = do
    pkgs <- withArgs (analyzePackages (analyzeFiles analyzers))
    writePackages (post pkgs)

withArgs :: MonadIO m => (FilePath -> m a) -> m a
withArgs child =
    join $ liftIO $ execParser $ info (helper <*> parser) fullDesc
  where
    parser = child <$> strArgument (metavar "MANIFEST")

writePackages :: MonadIO m => Map Text Package -> m ()
writePackages = liftIO . BL.writeFile "packages.json" . encodePretty
