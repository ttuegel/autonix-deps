module Archive (archiveList, archiveView) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (isSuffixOf)
import System.Process (readProcess)

archiveList :: MonadIO m => FilePath -> m [FilePath]
archiveList path
    | isTarball path = tarList path
    | otherwise = error $ "no archive handler for " ++ path

isTarball :: FilePath -> Bool
isTarball path =
    any (`isSuffixOf` path)
    [ ".tar"
    , ".tar.gz"
    , ".tar.bz2"
    , ".tar.xz"
    , ".tar.lzma"
    ]

tarList :: MonadIO m => FilePath -> m [FilePath]
tarList path = liftIO $ do
    filter (not . null) . lines <$> readProcess "tar" ["taf", path] ""

archiveView :: MonadIO m => FilePath -> FilePath -> m ByteString
archiveView path file
    | isTarball path = tarView path file
    | otherwise = error $ "no archive handler for " ++ path

tarView :: MonadIO m => FilePath -> FilePath -> m ByteString
tarView path file = liftIO $ do
    B.pack <$> readProcess "tar" ["Oxaf", path, file] ""
