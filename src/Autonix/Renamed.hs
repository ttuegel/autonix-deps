{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Renamed
       ( Renamed, renames, deps
       , HasDeps(..)
       , addBuildInput, addNativeBuildInput
       , addPropagatedBuildInput, addPropagatedNativeBuildInput
       , addPropagatedUserEnvPkg
       , rename
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as S

import Autonix.Deps hiding (deps)
import Autonix.PackageDeps

data Renamed =
    Renamed { _renames :: Map ByteString ByteString
            , _deps :: Map ByteString PackageDeps
            }
makeLenses ''Renamed

rename :: ByteString -> ByteString -> Renamed -> Renamed
rename old new = execState $ do
    renames %= M.insert old new
    deps %= M.mapKeys go
    deps %= M.map
        ( execState $ do
               _buildInputs %= S.map go
               _nativeBuildInputs %= S.map go
               _propagatedBuildInputs %= S.map go
               _propagatedNativeBuildInputs %= S.map go
               _propagatedUserEnvPkgs %= S.map go
        )
  where
    go nm | nm == old = new
          | otherwise = nm

addDeps :: MonadState Renamed m
        => ASetter' PackageDeps (Set ByteString) -> AddDeps m
addDeps l pkg inputs = do
    let rnm input = liftM (fromMaybe input . M.lookup input) $ use renames
    renamedInputs <- mapM rnm inputs
    let pdeps :: PackageDeps
        pdeps = mempty & l .~ S.fromList renamedInputs
    deps %= M.insertWith (<>) pkg pdeps

instance HasDeps Renamed where
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
