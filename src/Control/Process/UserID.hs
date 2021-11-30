{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Process.UserID(
  HasUserID(..)
, AsUserID(..)
) where

import Control.Category((.), id)
import Control.Lens ( iso, Lens', Prism' )
import Data.Word ( Word32 )
import System.Posix.Types ( CUid(CUid) )
import System.Process.Internals ( UserID )

class HasUserID a where
  userID ::
    Lens' a UserID
  userIDWord32 ::
    Lens' a Word32
  userIDWord32 =
    userID .
      iso
        (\(CUid x) -> x)
        CUid

instance HasUserID UserID where
  userID =
    id

class AsUserID a where
  _UserID ::
    Prism' a UserID
  _UserIDWord32 ::
    Prism' a Word32
  _UserIDWord32 =
    _UserID .
      iso
        (\(CUid x) -> x)
        CUid

instance AsUserID UserID where
  _UserID =
    id
