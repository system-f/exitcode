{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Process.FD(
  HasFD(..)
, AsFD(..)
) where

import Control.Category((.), id)
import Control.Lens ( iso, Lens', Prism' )
import Data.Int ( Int32 )
import Foreign.C.Types ( CInt(CInt) )
import System.Posix.Internals ( FD )

class HasFD a where
  fd ::
    Lens' a FD
  fdInt32 ::
    Lens' a Int32
  fdInt32 =
    fd .
      iso
        (\(CInt x) -> x)
        CInt

instance HasFD FD where
  fd =
    id

class AsFD a where
  _FD ::
    Prism' a FD
  _FDInt32 ::
    Prism' a Int32
  _FDInt32 =
    _FD .
      iso
        (\(CInt x) -> x)
        CInt

instance AsFD FD where
  _FD =
    id
