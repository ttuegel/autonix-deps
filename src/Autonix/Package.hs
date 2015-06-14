{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Package
       ( Package, package
       , name, src
       , buildInputs
       , nativeBuildInputs
       , propagatedBuildInputs
       , propagatedNativeBuildInputs
       , propagatedUserEnvPkgs
       ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics

import Autonix.Manifest (Src)

data Package =
  Package { _name :: Text
          , _src :: Src
          , _buildInputs :: Set Text
          , _nativeBuildInputs :: Set Text
          , _propagatedBuildInputs :: Set Text
          , _propagatedNativeBuildInputs :: Set Text
          , _propagatedUserEnvPkgs :: Set Text
          }
  deriving Generic
makeLenses ''Package

packageOptions :: Options
packageOptions = defaultOptions { fieldLabelModifier = ("_" ++) }

instance FromJSON Package where
  parseJSON = genericParseJSON packageOptions

instance ToJSON Package where
  toJSON = genericToJSON packageOptions

package :: Text -> Src -> Package
package n s =
  Package { _name = n
          , _src = s
          , _buildInputs = S.empty
          , _nativeBuildInputs = S.empty
          , _propagatedBuildInputs = S.empty
          , _propagatedNativeBuildInputs = S.empty
          , _propagatedUserEnvPkgs = S.empty
          }
