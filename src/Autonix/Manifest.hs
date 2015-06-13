{-# LANGUAGE DeriveGeneric #-}

module Autonix.Manifest (Manifest(..), Src(..), readManifest, readRenames) where

import Control.Applicative
import Control.Error
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import GHC.Generics

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

readManifest :: MonadIO m => FilePath -> m [Manifest]
readManifest path =
    liftM
    (fromMaybe $ error "malformed manifest.xml")
    (liftIO $ decode' <$> BL.readFile path)

readRenames :: MonadIO m => Maybe FilePath -> m (Map ByteString ByteString)
readRenames renamesPath = do
    case renamesPath of
        Nothing -> return M.empty
        Just path ->
            liftM (M.fromList . map (toPair . B.words) . B.lines)
            $ liftIO $ B.readFile path
  where
    toPair [old, new] = (old, new)
    toPair _ = error "readRenames: invalid line"
