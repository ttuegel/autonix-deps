{-# LANGUAGE TemplateHaskell #-}

module Autonix.PkgDeps
       ( PkgDeps
       , buildInputs, nativeBuildInputs
       , propagatedBuildInputs, propagatedNativeBuildInputs
       , propagatedUserEnvPkgs
       , module Data.ByteString
       , module Data.Map
       , module Data.Set
       ) where

import Control.Lens
import Control.Monad.State (execState)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Set as S

data PkgDeps =
    PkgDeps
    { _buildInputs :: Set Text
    , _nativeBuildInputs :: Set Text
    , _propagatedBuildInputs :: Set Text
    , _propagatedNativeBuildInputs :: Set Text
    , _propagatedUserEnvPkgs :: Set Text
    }
  deriving (Eq, Ord, Read, Show)
makeLenses ''PkgDeps

instance Monoid PkgDeps where
    mempty =
        PkgDeps
        { _buildInputs = S.empty
        , _nativeBuildInputs = S.empty
        , _propagatedBuildInputs = S.empty
        , _propagatedNativeBuildInputs = S.empty
        , _propagatedUserEnvPkgs = S.empty
        }

    mappend a = execState $ do
        buildInputs %= mappend (a^.buildInputs)
        nativeBuildInputs %= mappend (a^.nativeBuildInputs)
        propagatedBuildInputs %= mappend (a^.propagatedBuildInputs)
        propagatedNativeBuildInputs %= mappend (a^.propagatedNativeBuildInputs)
        propagatedUserEnvPkgs %= mappend (a^.propagatedUserEnvPkgs)
