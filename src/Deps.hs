{-# LANGUAGE OverloadedStrings #-}

module Deps
       ( generateDeps
       , module Types
       ) where

import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Set as S
import System.Environment (getArgs)

import Analyze
import Manifest
import Types

generateDeps :: [Analyzer (StateT Deps IO)] -> IO ()
generateDeps analyzers = do
    [manifestXml, dependenciesNix] <- getArgs
    manifest <- readManifest manifestXml
    deps <- analyzeFiles analyzers manifest
    B.writeFile dependenciesNix $ depsToNix deps

packageDeps :: (ByteString, Deps) -> ByteString
packageDeps (name, deps) =
    B.unlines
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
    listInputs f = B.unwords $ map quoted $ S.toList $ f deps
    quoted str = "\"" <> str <> "\""

depsToNix :: [(ByteString, Deps)] -> ByteString
depsToNix deps =
    B.unlines
    ( [ "{ }:"
      , "{"
      ]
      ++ map packageDeps deps ++
      [ "}" ]
    )
