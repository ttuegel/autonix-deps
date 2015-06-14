{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix.Generate (generateDeps, writeRenames) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Autonix.Analyze
import Autonix.Args
import Autonix.Deps
import Autonix.Renames

generateDeps :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer (StateT Renames m)]
             -> StateT Renames m ()
generateDeps analyzers = do
    pkgs <- withArgs $ analyzePackages (analyzeFiles analyzers)
    get >>= writeRenames

writeRenames :: MonadIO m => Renames -> m ()
writeRenames = liftIO . BL.writeFile "renames.json" . encode
