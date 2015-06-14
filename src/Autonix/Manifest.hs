{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Autonix.Manifest
       ( Manifest, name, store, src
       , readManifests
       ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Autonix.Src (Src)

data Manifest =
  Manifest { _name :: Text
           , _store :: FilePath
           , _src :: Src
           }
  deriving Generic
makeLenses ''Manifest

manifestOptions :: Options
manifestOptions = defaultOptions { fieldLabelModifier = tail }

instance FromJSON Manifest where
  parseJSON = genericParseJSON manifestOptions

instance ToJSON Manifest where
  toJSON = genericToJSON manifestOptions

readManifests :: MonadIO m => FilePath -> m (Map Text Manifest)
readManifests path = do
  emanifests <- eitherDecode' <$> liftIO (BL.readFile path)
  case emanifests :: Either String [Manifest] of
    Left err -> error ("readManifests: could not read manifest.json\n" ++ err)
    Right manifests -> return (foldr assemble M.empty manifests)
  where
    assemble :: Manifest -> Map Text Manifest -> Map Text Manifest
    assemble manifest =
      M.insertWith keepLatestVersion (onlyName $ view name manifest) manifest

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
      case compare (l^.name) (r^.name) of
        LT -> r
        EQ -> l
        GT -> l
