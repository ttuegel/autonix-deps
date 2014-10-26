{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Deps
       ( AddDeps, AddDep
       , Deps, deps, HasDeps(..)
       , buildInputs, nativeBuildInputs
       , propagatedBuildInputs, propagatedNativeBuildInputs
       , propagatedUserEnvPkgs
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import Autonix.PackageDeps

type AddDeps m = ByteString -> [ByteString] -> m ()
type AddDep m = ByteString -> ByteString -> m ()
type PerPkg = Map ByteString (Set ByteString)

depLens :: Lens' a (Map ByteString PackageDeps)
        -> Lens' PackageDeps (Set ByteString) -> Lens' a PerPkg
depLens dl l = lens getter setter
  where
    getter = view $ dl . to (M.map $ view l)
    setter d pp = d & dl %~ (M.mapWithKey $ \pkg -> execState $ do
        l .= fromMaybe S.empty (M.lookup pkg pp))

class HasDeps a where
    hasDeps :: Lens' a (Map ByteString PackageDeps)

buildInputs :: HasDeps a => Lens' a PerPkg
buildInputs = depLens hasDeps _buildInputs

nativeBuildInputs :: HasDeps a => Lens' a PerPkg
nativeBuildInputs = depLens hasDeps _nativeBuildInputs

propagatedBuildInputs :: HasDeps a => Lens' a PerPkg
propagatedBuildInputs = depLens hasDeps _propagatedBuildInputs

propagatedNativeBuildInputs :: HasDeps a => Lens' a PerPkg
propagatedNativeBuildInputs = depLens hasDeps _propagatedNativeBuildInputs

propagatedUserEnvPkgs :: HasDeps a => Lens' a PerPkg
propagatedUserEnvPkgs = depLens hasDeps _propagatedUserEnvPkgs

newtype Deps = Deps { _deps :: Map ByteString PackageDeps }
makeLenses ''Deps

instance HasDeps Deps where
    hasDeps = deps
