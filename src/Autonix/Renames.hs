{-# LANGUAGE FlexibleContexts #-}

module Autonix.Renames where

import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)

type Renames = Map Text Text

writeRenames :: MonadIO m => Renames -> m ()
writeRenames = liftIO . BL.writeFile "renames.json" . encodePretty
