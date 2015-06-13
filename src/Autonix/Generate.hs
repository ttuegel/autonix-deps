{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix.Generate (generateDeps, writeDeps, writeRenames) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Autonix.Analyze
import Autonix.Args
import Autonix.Deps

generateDeps :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
             => [Analyzer (ResourceT (StateT Deps m))]
             -> StateT Deps m ()
generateDeps analyzers = do
    withArgs $ analyzePackages (analyzeFiles analyzers)
    get >>= writeDeps
    get >>= writeRenames

writeDeps :: MonadIO m => Deps -> m ()
writeDeps = liftIO . T.writeFile "dependencies.nix" . depsToNix

writeRenames :: MonadIO m => Deps -> m ()
writeRenames ds = liftIO $ do
  let renames = do
        (old, new) <- M.toList (ds^.names)
        return ("\"" <> old <> "\" = \"" <> new <> "\";")
  T.writeFile
    "renames.nix"
    (T.unlines
      ([ "# DO NOT EDIT! This file is generated automatically."
       , "{ }:"
       , "{"
       ]
       ++ renames ++
       [ "}" ]))

packageDeps :: (Text, PkgDeps) -> Text
packageDeps (name, ds) =
    T.unlines
    [ "  " <> name <> " = {"
    , "    buildInputs = [ "
      <> listInputs buildInputs
      <> " ];"
    , "    nativeBuildInputs = [ "
      <> listInputs nativeBuildInputs
      <> " ];"
    , "    propagatedBuildInputs = [ "
      <> listInputs propagatedBuildInputs
      <> " ];"
    , "    propagatedNativeBuildInputs = [ "
      <> listInputs propagatedNativeBuildInputs
      <> " ];"
    , "    propagatedUserEnvPkgs = [ "
      <> listInputs propagatedUserEnvPkgs
      <> " ];"
    , "  };"
    ]
  where
    listInputs l = T.unwords $ map quoted $ S.toList $ ds^.l
    quoted x = "\"" <> x <> "\""

depsToNix :: Deps -> Text
depsToNix (view deps -> ds) =
    T.unlines
    ( [ "# DO NOT EDIT! This file is generated automatically."
      , "{ }:"
      , "{"
      ]
      ++ map packageDeps (M.toList ds) ++
      [ "}" ]
    )
