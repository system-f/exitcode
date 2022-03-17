{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Process.Pid(
  HasPid(..)
, AsPid(..)
) where

import Control.Category((.), id)
import Control.Lens ( iso, Lens', Prism' )
import Data.Int ( Int32 )
import System.Posix.Types ( CPid(CPid) )
import System.Process ( Pid )

class HasPid a where
  pid ::
    Lens' a Pid
  pidInt32 ::
    Lens' a Int32
  pidInt32 =
    pid .
      iso
        (\(CPid x) -> x)
        CPid

instance HasPid Pid where
  pid =
    id

class AsPid a where
  _Pid ::
    Prism' a Pid
  _PidInt32 ::
    Prism' a Int32
  _PidInt32 =
    _Pid .
      iso
        (\(CPid x) -> x)
        CPid

instance AsPid Pid where
  _Pid =
    id
