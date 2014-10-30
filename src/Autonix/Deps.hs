{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Autonix.Deps
       ( Deps, names, deps
       , add, rename
       , module Autonix.PkgDeps
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Set as S
import Prelude hiding (foldr)

import Autonix.PkgDeps

data Deps =
    Deps { _names :: Map ByteString ByteString
         , _deps :: Map ByteString PkgDeps
         }
makeLenses ''Deps

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

type instance Index Deps = ByteString
type instance IxValue Deps = PkgDeps

instance Ixed Deps where
    ix idx f r = (deps . ix (lookupNewName r idx)) f r

instance At Deps where
    at idx f r = (deps . at (lookupNewName r idx)) f r

add :: (At s, Eq a, Monoid a, IxValue s ~ a) => Index s -> Lens' s a
add idx =
    at idx
    . iso (fromMaybe mempty) (\a -> if a == mempty then Nothing else Just a)
