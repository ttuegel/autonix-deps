module Args where

import System.Environment (getArgs)

getManifestNix :: IO FilePath
getManifestNix = do
    [path, _] <- getArgs
    return path

getOutDir :: IO FilePath
getOutDir = do
    [_, dir] <- getArgs
    return dir
