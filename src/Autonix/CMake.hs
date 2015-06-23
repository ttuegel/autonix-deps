{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Autonix.CMake
       ( analyzeCMakePackages
       , analyzeCMakePrograms
       , cmakeAnalyzers
       , detectCMake
       ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Conduit
import Data.Map (Map)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Autonix.Analyze
import Autonix.Package (Package)
import qualified Autonix.Package as Package
import Autonix.Regex
import Autonix.Renames

detectCMake :: (Monad m, MonadState (Map Text Package, Renames) m) => Analyzer m
detectCMake pkg _ = awaitForever $ \(path, _) ->
    when (takeFileName path == "CMakeLists.txt")
        $ _1 . ix pkg . Package.nativeBuildInputs %= S.insert "cmake"

analyzeCMakePackages :: (MonadIO m, MonadState (Map Text Package, Renames) m) => Analyzer m
analyzeCMakePackages pkg _ = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = S.fromList
                  $ map (T.toLower . T.decodeUtf8)
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        _1 . ix pkg . Package.buildInputs <>= new

analyzeCMakePrograms :: (MonadIO m, MonadState (Map Text Package, Renames) m) => Analyzer m
analyzeCMakePrograms pkg _ = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = S.fromList
                  $ map (T.toLower . T.decodeUtf8)
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_program[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        _1 . ix pkg . Package.nativeBuildInputs <>= new

cmakeAnalyzers :: (MonadIO m, MonadState (Map Text Package, Renames) m) => [Analyzer m]
cmakeAnalyzers = [detectCMake, analyzeCMakePackages, analyzeCMakePrograms]
