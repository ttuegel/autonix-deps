{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Deps
       ( AddDeps, AddDep
       , Deps, deps, HasDeps(..)
       , addBuildInput, addNativeBuildInput
       , addPropagatedBuildInput, addPropagatedNativeBuildInput
       , addPropagatedUserEnvPkg
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

import Autonix.PackageDeps

type AddDeps m = ByteString -> [ByteString] -> m ()
type AddDep m = ByteString -> ByteString -> m ()
type PerPkg = Map ByteString (Set ByteString)
type GetDeps a = Getting PerPkg a PerPkg

class HasDeps a where
    buildInputs :: GetDeps a
    nativeBuildInputs :: GetDeps a
    propagatedBuildInputs :: GetDeps a
    propagatedNativeBuildInputs :: GetDeps a
    propagatedUserEnvPkgs :: GetDeps a
    addBuildInputs :: MonadState a m => AddDeps m
    addNativeBuildInputs :: MonadState a m => AddDeps m
    addPropagatedBuildInputs :: MonadState a m => AddDeps m
    addPropagatedNativeBuildInputs :: MonadState a m => AddDeps m
    addPropagatedUserEnvPkgs :: MonadState a m => AddDeps m

newtype Deps = Deps { _deps :: Map ByteString PackageDeps }
makeLenses ''Deps

addDeps :: MonadState Deps m
        => ASetter' PackageDeps (Set ByteString) -> AddDeps m
addDeps l pkg inputs = do
    let pdeps :: PackageDeps
        pdeps = mempty & l .~ S.fromList inputs
    deps %= M.insertWith (<>) pkg pdeps

addDep :: (HasDeps a, MonadState a m) => AddDeps m -> AddDep m
addDep f pkg input = f pkg [input]

instance HasDeps Deps where
    buildInputs = deps . to (M.map $ view _buildInputs)
    nativeBuildInputs = deps . to (M.map $ view _nativeBuildInputs)
    propagatedBuildInputs =
        deps . to (M.map $ view _propagatedBuildInputs)
    propagatedNativeBuildInputs =
        deps . to (M.map $ view _propagatedNativeBuildInputs)
    propagatedUserEnvPkgs =
        deps . to (M.map $ view _propagatedUserEnvPkgs)
    addBuildInputs = addDeps _buildInputs
    addNativeBuildInputs = addDeps _nativeBuildInputs
    addPropagatedBuildInputs = addDeps _propagatedBuildInputs
    addPropagatedNativeBuildInputs = addDeps _propagatedNativeBuildInputs
    addPropagatedUserEnvPkgs = addDeps _propagatedUserEnvPkgs

addBuildInput :: (HasDeps a, MonadState a m) => AddDep m
addBuildInput = addDep addBuildInputs

addNativeBuildInput :: (HasDeps a, MonadState a m) => AddDep m
addNativeBuildInput = addDep addNativeBuildInputs

addPropagatedBuildInput :: (HasDeps a, MonadState a m) => AddDep m
addPropagatedBuildInput = addDep addPropagatedBuildInputs

addPropagatedNativeBuildInput :: (HasDeps a, MonadState a m) => AddDep m
addPropagatedNativeBuildInput = addDep addPropagatedNativeBuildInputs

addPropagatedUserEnvPkg :: (HasDeps a, MonadState a m) => AddDep m
addPropagatedUserEnvPkg = addDep addPropagatedUserEnvPkgs
