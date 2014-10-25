{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Generate (generateDeps, writeDeps) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

import Analyze
import Deps
import PackageDeps

generateDeps :: MonadIO m => [Analyzer (StateT Deps m)] -> StateT Deps m ()
generateDeps analyzers = do
    analyzePackages (analyzeFiles analyzers)
    get >>= writeDeps

writeDeps :: MonadIO m => Deps -> m ()
writeDeps = liftIO . B.writeFile "dependencies.nix" . depsToNix

packageDeps :: (ByteString, PackageDeps) -> ByteString
packageDeps (name, ds) =
    B.unlines
    [ "  " <> name <> " = {"
    , "    buildInputs = [ "
      <> listInputs _buildInputs
      <> " ];"
    , "    nativeBuildInputs = [ "
      <> listInputs _nativeBuildInputs
      <> " ];"
    , "    propagatedBuildInputs = [ "
      <> listInputs _propagatedBuildInputs
      <> " ];"
    , "    propagatedNativeBuildInputs = [ "
      <> listInputs _propagatedNativeBuildInputs
      <> " ];"
    , "    propagatedUserEnvPkgs = [ "
      <> listInputs _propagatedUserEnvPkgs
      <> " ];"
    , "  };"
    ]
  where
    listInputs l = B.unwords $ map quoted $ S.toList $ ds^.l
    quoted str = "\"" <> str <> "\""

depsToNix :: Deps -> ByteString
depsToNix (view deps -> ds) =
    B.unlines
    ( [ "{ }:"
      , "{"
      ]
      ++ map packageDeps (M.toList ds) ++
      [ "}" ]
    )
