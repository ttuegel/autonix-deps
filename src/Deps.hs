module Deps
       ( generateDeps
       , module Types
       ) where

import Control.Monad.IO.Class
import Control.Monad.State

import Analyze
import Generate
import Types

generateDeps :: MonadIO m => [Analyzer (StateT Deps m)] -> m ()
generateDeps analyzers =
    analyzePackages (analyzeFiles analyzers) >>= writeDeps
