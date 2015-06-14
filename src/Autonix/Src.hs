{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Src (Src, mkSrc, url, sha256, name) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

data Src =
  Src { _url :: FilePath
      , _sha256 :: Text
      , _name :: Text
      }
  deriving Generic
makeLenses ''Src

srcOptions :: Options
srcOptions = defaultOptions { fieldLabelModifier = tail }

instance FromJSON Src where
  parseJSON = genericParseJSON srcOptions

instance ToJSON Src where
  toJSON = genericToJSON srcOptions

mkSrc :: FilePath -> Text -> Text -> Src
mkSrc = Src
