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
import qualified Data.Set as S

import Autonix.Analyze
import Autonix.Deps
import Autonix.Regex

detectCMake :: MonadState Deps m => Analyzer m
detectCMake pkg =
    matchFileName "CMakeLists.txt" $ const
    $ nativeBuildInputs . ix pkg %= S.insert "cmake"

analyzeCMakePackages :: (MonadIO m, MonadState Deps m) => Analyzer m
analyzeCMakePackages pkg = matchFileName "CMakeLists.txt" $ \getFile -> do
    contents <- liftIO getFile
    let new = concatMap (take 1 . drop 1) $ match regex contents
        regex = makeRegex "find_package\\([[:space:]]*([^[:space:],$\\)]+)"
    buildInputs . ix pkg %= S.union (S.fromList new)

analyzeCMakePrograms :: (MonadIO m, MonadState Deps m) => Analyzer m
analyzeCMakePrograms pkg = matchFileName "CMakeLists.txt" $ \getFile -> do
    contents <- liftIO getFile
    let new = concatMap (take 1 . drop 1) $ match regex contents
        regex = makeRegex "find_program\\([[:space:]]*([^[:space:],$\\)]+)"
    nativeBuildInputs . ix pkg %= S.union (S.fromList new)

cmakeAnalyzers :: (MonadIO m, MonadState Deps m) => [Analyzer m]
cmakeAnalyzers = [detectCMake, analyzeCMakePackages, analyzeCMakePrograms]
