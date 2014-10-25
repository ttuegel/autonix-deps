module Autonix.Args where

import Control.Applicative
import System.Environment (getArgs)

getManifestNix :: IO FilePath
getManifestNix = head <$> getArgs
