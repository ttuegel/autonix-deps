{-# LANGUAGE OverloadedStrings #-}

module Generate where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B
import Data.Monoid
import qualified Data.Set as S

import Types

writeDeps :: MonadIO m => [(ByteString, Deps)] -> m ()
writeDeps deps = liftIO $ do
    B.writeFile "dependencies.nix" $ depsToNix deps

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
    listInputs l = B.unwords $ map quoted $ S.toList $ deps^.l
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
