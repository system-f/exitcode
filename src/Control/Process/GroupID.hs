{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Process.GroupID(
  HasGroupID(..)
, AsGroupID(..)
) where

import Control.Category(id)
import Control.Lens ( Lens', Prism' )
import System.Process.Internals ( GroupID )

class HasGroupID a where
  groupID ::
    Lens' a GroupID

instance HasGroupID GroupID where
  groupID =
    id

class AsGroupID a where
  _GroupID ::
    Prism' a GroupID

instance AsGroupID GroupID where
  _GroupID =
    id
