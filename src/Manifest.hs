module Manifest (readManifest) where

import Control.Applicative
import Control.Error
import Control.Monad (liftM)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Text.XML.Light

import Args
import Types

readManifest :: MonadIO m => m Manifest
readManifest =
    liftM (fromMaybe $ error "malformed manifest.xml")
    $ liftIO $ do
        path <- getManifestNix
        runMaybeT $ do
            xml <- MaybeT $ parseXMLDoc <$> B.readFile path
            attrs <-
                MaybeT $ return
                $ if qName (elName xml) == "expr"
                  then case headMay (elChildren xml) of
                      Just el | qName (elName el) == "attrs" -> Just el
                      _ -> Nothing
                  else Nothing
            return $ attrsToManifest attrs

attrsToManifest :: Element -> Manifest
attrsToManifest = mapMaybe go . filterChildrenName isAttr where
  isAttr name = qName name == "attr"
  go el = do
      name <- findAttr (blank_name { qName = "name" }) el
      child <- headMay $ elChildren el
      value <- findAttr (blank_name { qName = "value"}) child
      return (B.pack name, value)
