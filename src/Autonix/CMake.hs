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
import qualified Autonix.Package as Package
import Autonix.Regex

detectCMake :: Monad m => Analyzer m
detectCMake _ = awaitForever $ \(path, _) ->
    when (takeFileName path == "CMakeLists.txt")
        $ Package.nativeBuildInputs %= S.insert "cmake"

analyzeCMakePackages :: MonadIO m => Analyzer m
analyzeCMakePackages _ = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = map T.decodeUtf8
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        Package.buildInputs %= S.union (S.fromList new)

analyzeCMakePrograms :: MonadIO m => Analyzer m
analyzeCMakePrograms _ = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = map T.decodeUtf8
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_program[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        Package.nativeBuildInputs %= S.union (S.fromList new)

cmakeAnalyzers :: MonadIO m => [Analyzer m]
cmakeAnalyzers = [detectCMake, analyzeCMakePackages, analyzeCMakePrograms]
