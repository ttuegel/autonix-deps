module Types
       ( Analyzer
       , Deps(..)
       , Manifest
       , module Data.ByteString
       , module Data.Set
       ) where

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S

data Deps =
    Deps
    { buildInputs :: Set ByteString
    , nativeBuildInputs :: Set ByteString
    , propagatedBuildInputs :: Set ByteString
    , propagatedNativeBuildInputs :: Set ByteString
    , propagatedUserEnvPkgs :: Set ByteString
    }

instance Monoid Deps where
    mempty =
        Deps
        { buildInputs = S.empty
        , nativeBuildInputs = S.empty
        , propagatedBuildInputs = S.empty
        , propagatedNativeBuildInputs = S.empty
        , propagatedUserEnvPkgs = S.empty
        }

    mappend a b =
        Deps
        { buildInputs = (mappend `on` buildInputs) a b
        , nativeBuildInputs = (mappend `on` nativeBuildInputs) a b
        , propagatedBuildInputs = (mappend `on` propagatedBuildInputs) a b
        , propagatedNativeBuildInputs =
            (mappend `on` propagatedNativeBuildInputs) a b
        , propagatedUserEnvPkgs = (mappend `on` propagatedUserEnvPkgs) a b
        }

type Analyzer m = FilePath -> Maybe (ByteString -> m ())

type Manifest = [(ByteString, FilePath)]
