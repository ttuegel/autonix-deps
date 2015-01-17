{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Autonix.Deps
       ( Deps, names, deps, rename
       , module Autonix.PkgDeps
       ) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
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

rename :: (MonadState Deps m) => ByteString -> ByteString -> m ()
rename old new = names %= M.insert old new

type instance Index Deps = ByteString
type instance IxValue Deps = PkgDeps

instance Ixed Deps where
    ix idx = deps . ix idx

instance At Deps where
    at idx = deps . at idx
