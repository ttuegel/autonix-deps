{-# LANGUAGE TemplateHaskell #-}

module PackageDeps
       ( PackageDeps
       , _buildInputs, _nativeBuildInputs
       , _propagatedBuildInputs, _propagatedNativeBuildInputs
       , _propagatedUserEnvPkgs
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
import qualified Data.Set as S

data PackageDeps =
    PackageDeps
    { __buildInputs :: Set ByteString
    , __nativeBuildInputs :: Set ByteString
    , __propagatedBuildInputs :: Set ByteString
    , __propagatedNativeBuildInputs :: Set ByteString
    , __propagatedUserEnvPkgs :: Set ByteString
    }
makeLenses ''PackageDeps

instance Monoid PackageDeps where
    mempty =
        PackageDeps
        { __buildInputs = S.empty
        , __nativeBuildInputs = S.empty
        , __propagatedBuildInputs = S.empty
        , __propagatedNativeBuildInputs = S.empty
        , __propagatedUserEnvPkgs = S.empty
        }

    mappend a = execState $ do
        _buildInputs %= mappend (a^._buildInputs)
        _nativeBuildInputs %= mappend (a^._nativeBuildInputs)
        _propagatedBuildInputs %= mappend (a^._propagatedBuildInputs)
        _propagatedNativeBuildInputs %= mappend (a^._propagatedNativeBuildInputs)
        _propagatedUserEnvPkgs %= mappend (a^._propagatedUserEnvPkgs)
