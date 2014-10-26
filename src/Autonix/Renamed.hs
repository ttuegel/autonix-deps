{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Renamed
       ( Renamed, renames, deps
       , HasDeps(..)
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

instance HasDeps Renamed where
    hasDeps = lens (view deps) setter
      where
        setter ds pp = ds & over deps (M.mapWithKey $ \pkg _ ->
            renameAll (ds^.renames) $ fromMaybe mempty (M.lookup pkg pp))

renameAll :: Map ByteString ByteString -> PackageDeps -> PackageDeps
renameAll names = execState $ do
    _buildInputs %= S.map rnm
    _nativeBuildInputs %= S.map rnm
    _propagatedBuildInputs %= S.map rnm
    _propagatedNativeBuildInputs %= S.map rnm
    _propagatedUserEnvPkgs %= S.map rnm
  where
    rnm nm = fromMaybe nm $ M.lookup nm names
