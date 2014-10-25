{-# LANGUAGE OverloadedStrings #-}

module Deps
       ( generateDeps
       , module Types
       ) where

import Control.Monad.State

import Analyze
import Generate
import Types

generateDeps :: [Analyzer (StateT Deps IO)] -> IO ()
generateDeps analyzers =
    analyzePackages (analyzeFiles analyzers) >>= writeDeps
