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
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Autonix.Analyze
import Autonix.Deps
import Autonix.Regex

detectCMake :: MonadState Deps m => Analyzer m
detectCMake pkg = awaitForever $ \(path, _) -> do
    when (takeFileName path == "CMakeLists.txt")
        $ ix pkg . nativeBuildInputs %= S.insert "cmake"

analyzeCMakePackages :: (MonadIO m, MonadState Deps m) => Analyzer m
analyzeCMakePackages pkg = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = map T.decodeUtf8 $ concatMap (take 1 . drop 1) $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        ix pkg . buildInputs %= S.union (S.fromList new)

analyzeCMakePrograms :: (MonadIO m, MonadState Deps m) => Analyzer m
analyzeCMakePrograms pkg = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = map T.decodeUtf8 $ concatMap (take 1 . drop 1) $ match regex contents
            regex = makeRegex
                    "find_program[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        ix pkg . nativeBuildInputs %= S.union (S.fromList new)

cmakeAnalyzers :: (MonadIO m, MonadState Deps m) => [Analyzer m]
cmakeAnalyzers = [detectCMake, analyzeCMakePackages, analyzeCMakePrograms]
