{-# LANGUAGE TemplateHaskell #-}

module Types
       ( Analyzer
       , Deps, buildInputs, nativeBuildInputs
       , propagatedBuildInputs, propagatedNativeBuildInputs
       , propagatedUserEnvPkgs
       , Manifest
       , module Data.ByteString
       , module Data.Set
       ) where

import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S

data Deps =
    Deps
    { _buildInputs :: Set ByteString
    , _nativeBuildInputs :: Set ByteString
    , _propagatedBuildInputs :: Set ByteString
    , _propagatedNativeBuildInputs :: Set ByteString
    , _propagatedUserEnvPkgs :: Set ByteString
    }
makeLenses ''Deps

instance Monoid Deps where
    mempty =
        Deps
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

type Analyzer m = FilePath -> IO ByteString -> m ()

type Manifest = [(ByteString, FilePath)]
