{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Autonix.CMake
       ( analyzeCMakeLists
       , analyzeCMakePackages
       , analyzeCMakePrograms
       , cmakeAnalyzers
       , detectCMake
       ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Conduit
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import System.FilePath (takeFileName)

import Autonix.Analyze
import Autonix.Package (Package)
import qualified Autonix.Package as Package
import Autonix.Regex

detectCMake :: Monad m => Analyzer m
detectCMake _ = awaitForever $ \(path, _) ->
    when (takeFileName path == "CMakeLists.txt")
        $ Package.nativeBuildInputs %= S.insert "cmake"

analyzeCMakeLists :: MonadIO m => (Text -> Package -> Package) -> Analyzer m
analyzeCMakeLists addDep _ = awaitForever $ \(path, contents) -> do
    when (takeFileName path == "CMakeLists.txt") $ do
        let new = map T.decodeUtf8
                  $ concatMap (take 1 . drop 1)
                  $ match regex contents
            regex = makeRegex
                    "find_package[[:space:]]*\\([[:space:]]*([^[:space:],$\\)]+)"
        mapM_ (modify . addDep) new

analyzeCMakePackages :: MonadIO m => Analyzer m
analyzeCMakePackages =
  analyzeCMakeLists (\dep -> Package.buildInputs %~ S.insert dep)

analyzeCMakePrograms :: MonadIO m => Analyzer m
analyzeCMakePrograms =
  analyzeCMakeLists (\dep -> Package.nativeBuildInputs %~ S.insert dep)

cmakeAnalyzers :: MonadIO m => [Analyzer m]
cmakeAnalyzers = [detectCMake, analyzeCMakePackages, analyzeCMakePrograms]
