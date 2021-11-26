{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Control.Process.Handle(
  HasHandle(..)
, AsHandle(..)
) where

import Control.Category(id)
import Control.Lens ( prism', Lens', Prism' )
import Data.Maybe ( Maybe(Nothing, Just) )
import System.IO ( Handle )
import System.Process ( StdStream(UseHandle) )

class HasHandle a where
  handle ::
    Lens' a Handle

instance HasHandle Handle where
  handle =
    id

class AsHandle a where
  _Handle ::
    Prism' a Handle

instance AsHandle Handle where
  _Handle =
    id

instance AsHandle StdStream where
  _Handle =
    prism'
      UseHandle
      (\case
        UseHandle a -> Just a
        _ -> Nothing
      )
