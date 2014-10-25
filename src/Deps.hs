{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Deps
       ( Deps, deps, HasDeps(..)
       , addBuildInput, addNativeBuildInput
       , addPropagatedBuildInput, addPropagatedNativeBuildInput
       , addPropagatedUserEnvPkg
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

import PackageDeps

class HasDeps a where
    addBuildInputs :: MonadState a m => ByteString -> [ByteString] -> m ()
    addNativeBuildInputs :: MonadState a m => ByteString -> [ByteString] -> m ()
    addPropagatedBuildInputs :: MonadState a m => ByteString -> [ByteString] -> m ()
    addPropagatedNativeBuildInputs :: MonadState a m => ByteString -> [ByteString] -> m ()
    addPropagatedUserEnvPkgs :: MonadState a m => ByteString -> [ByteString] -> m ()

newtype Deps = Deps { _deps :: Map ByteString PackageDeps }
makeLenses ''Deps

addDeps :: MonadState Deps m
        => ASetter' PackageDeps (Set ByteString)
        -> ByteString -> [ByteString] -> m ()
addDeps l pkg inputs = do
    let pdeps :: PackageDeps
        pdeps = mempty & l .~ S.fromList inputs
    deps %= M.insertWith (<>) pkg pdeps

addDep :: (HasDeps a, MonadState a m)
       => (ByteString -> [ByteString] -> m ())
       -> ByteString -> ByteString -> m ()
addDep f pkg input = f pkg [input]

instance HasDeps Deps where
    addBuildInputs = addDeps _buildInputs
    addNativeBuildInputs = addDeps _nativeBuildInputs
    addPropagatedBuildInputs = addDeps _propagatedBuildInputs
    addPropagatedNativeBuildInputs = addDeps _propagatedNativeBuildInputs
    addPropagatedUserEnvPkgs = addDeps _propagatedUserEnvPkgs

addBuildInput :: (HasDeps a, MonadState a m)
              => ByteString -> ByteString -> m ()
addBuildInput = addDep addBuildInputs

addNativeBuildInput :: (HasDeps a, MonadState a m)
                    => ByteString -> ByteString -> m ()
addNativeBuildInput = addDep addNativeBuildInputs

addPropagatedBuildInput :: (HasDeps a, MonadState a m)
                        => ByteString -> ByteString -> m ()
addPropagatedBuildInput = addDep addPropagatedBuildInputs

addPropagatedNativeBuildInput :: (HasDeps a, MonadState a m)
                              => ByteString -> ByteString -> m ()
addPropagatedNativeBuildInput = addDep addPropagatedNativeBuildInputs

addPropagatedUserEnvPkg :: (HasDeps a, MonadState a m)
                        => ByteString -> ByteString -> m ()
addPropagatedUserEnvPkg = addDep addPropagatedUserEnvPkgs
