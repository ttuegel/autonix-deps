{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Autonix.Deps
       ( Deps, names, deps, rename
       , module Autonix.PkgDeps
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Prelude hiding (foldr)

import Autonix.PkgDeps

data Deps =
    Deps { _names :: Map ByteString ByteString
         , _deps :: Map ByteString PkgDeps
         }
  deriving (Read, Show)
makeLenses ''Deps

instance Monoid Deps where
    mempty = Deps { _names = M.empty, _deps = M.empty }

    mappend a b = flip execState a $ do
        let b' = execState (iforMOf_ (names.>itraversed) a rename) b
        iforMOf_ (names.>itraversed) b' rename
        names %= M.union (b'^.names)
        deps %= M.unionWith mappend (b'^.deps)

lookupNewName :: Deps -> ByteString -> ByteString
lookupNewName r idx = M.findWithDefault idx idx (r^.names)

renamePkgDeps :: ByteString -> ByteString -> PkgDeps -> PkgDeps
renamePkgDeps old new = execState $ do
    buildInputs %= S.map go
    nativeBuildInputs %= S.map go
    propagatedBuildInputs %= S.map go
    propagatedNativeBuildInputs %= S.map go
    propagatedUserEnvPkgs %= S.map go
  where
    go pkg | pkg == old = new
           | otherwise = pkg

rename :: (MonadState Deps m) => ByteString -> ByteString -> m ()
rename old new = do
    names %= M.insert old new
    names %= M.map (\tgt -> if tgt == old then new else tgt)
    deps %= M.map (renamePkgDeps old new)

applyRenames :: Deps -> PkgDeps -> PkgDeps
applyRenames r = execState $ do
    buildInputs %= S.map (lookupNewName r)
    propagatedBuildInputs %= S.map (lookupNewName r)
    nativeBuildInputs %= S.map (lookupNewName r)
    propagatedNativeBuildInputs %= S.map (lookupNewName r)
    propagatedUserEnvPkgs %= S.map (lookupNewName r)

type instance Index Deps = ByteString
type instance IxValue Deps = PkgDeps

instance Ixed Deps where
    ix idx f r =
        (deps . ix (lookupNewName r idx)) (fmap (applyRenames r) . f) r

instance At Deps where
    at idx f r =
        (deps . at (lookupNewName r idx)) (fmap (fmap $ applyRenames r) . f) r
