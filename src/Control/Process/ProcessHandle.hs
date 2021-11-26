{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.ProcessHandle(
  HasProcessHandle(..)
, AsProcessHandle(..)
) where

import Control.Category(id)
import System.Process ( ProcessHandle )
import Control.Lens ( Lens', Prism' )

class HasProcessHandle a where
  processHandle ::
    Lens' a ProcessHandle

instance HasProcessHandle ProcessHandle where
  processHandle =
    id

class AsProcessHandle a where
  _ProcessHandle ::
    Prism' a ProcessHandle

instance AsProcessHandle ProcessHandle where
  _ProcessHandle =
    id
