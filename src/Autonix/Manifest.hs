{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Autonix.Manifest (Manifest(..), Src(..), readManifests, readRenames) where

import Control.Applicative
import Control.Error
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Autonix.Renames

data Src =
  Src { src_url :: FilePath
      , src_sha256 :: Text
      , src_name :: Text
      }
  deriving Generic

srcOptions :: Options
srcOptions = defaultOptions { fieldLabelModifier = ("src_" ++) }

instance FromJSON Src where
  parseJSON = genericParseJSON srcOptions

instance ToJSON Src where
  toJSON = genericToJSON srcOptions

data Manifest =
  Manifest { manifest_name :: Text
           , manifest_store :: FilePath
           , manifest_src :: Src
           }
  deriving Generic

manifestOptions :: Options
manifestOptions = defaultOptions { fieldLabelModifier = ("manifest_" ++) }

instance FromJSON Manifest where
  parseJSON = genericParseJSON manifestOptions

instance ToJSON Manifest where
  toJSON = genericToJSON manifestOptions

readManifests :: MonadIO m => FilePath -> m (Map Text Manifest)
readManifests path = do
  mmanifests <- decode' <$> liftIO (BL.readFile path)
  case mmanifests :: Maybe [Manifest] of
    Nothing -> error "readManifests: could not read manifest.json"
    Just manifests -> return (foldr assemble M.empty manifests)
  where
    assemble :: Manifest -> Map Text Manifest -> Map Text Manifest
    assemble manifest =
      M.insertWith keepLatestVersion (onlyName $ manifest_name manifest) manifest
    onlyName :: Text -> Text
    onlyName nameAndVersion =
      T.intercalate "-"
      $ takeWhile headNotDigit
      $ T.splitOn "-" nameAndVersion
    headNotDigit :: Text -> Bool
    headNotDigit txt | T.null txt = True
                     | otherwise = Char.isDigit (T.head txt)
    keepLatestVersion :: Manifest -> Manifest -> Manifest
    keepLatestVersion l r =
      case compare (manifest_name l) (manifest_name r) of
        LT -> r
        EQ -> l
        GT -> l

readRenames :: MonadIO m => Maybe FilePath -> m Renames
readRenames Nothing = return M.empty
readRenames (Just path) = do
  mrenames <- decode' <$> liftIO (BL.readFile path)
  case mrenames of
    Nothing -> return M.empty
    Just renames -> return renames
