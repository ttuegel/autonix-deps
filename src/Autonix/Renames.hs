{-# LANGUAGE FlexibleContexts #-}

module Autonix.Renames where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson (decode')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)

import Autonix.Package (Package)
import qualified Autonix.Package as Package

type Renames = Map Text Text

applyRenames :: Renames -> Map Text Package -> Map Text Package
applyRenames renames = M.mapKeys renamePackage . M.map renameDependencies
  where
    renamePackage name = fromMaybe name (M.lookup name renames)
    renameDependencies =
      (Package.buildInputs %~ S.map renamePackage)
      . (Package.propagatedBuildInputs %~ S.map renamePackage)
      . (Package.nativeBuildInputs %~ S.map renamePackage)
      . (Package.propagatedNativeBuildInputs %~ S.map renamePackage)
      . (Package.propagatedUserEnvPkgs %~ S.map renamePackage)

readRenames :: MonadIO m => Maybe FilePath -> m Renames
readRenames Nothing = return M.empty
readRenames (Just path) =
  fromMaybe M.empty . decode' <$> liftIO (BL.readFile path)

writeRenames :: MonadIO m => Renames -> m ()
writeRenames = liftIO . BL.writeFile "renames.json" . encodePretty

rename :: MonadState Renames m => Text -> Text -> m ()
rename old new = modify (M.insert old new)
