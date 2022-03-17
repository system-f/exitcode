{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Control.Process.StdStream(
  StdStream(..)
, AsStdStream(..)
, HasStdStream(..)
) where

import Control.Category ( Category(id, (.)) )
import Control.Lens ( prism', Lens', Prism' )
import Data.Maybe ( Maybe(Nothing, Just) )
import System.Process ( StdStream(..) )

class AsStdStream a where
  _StdStream ::
    Prism' a StdStream
  _Inherit ::
    Prism' a ()
  _Inherit =
    _StdStream . _Inherit
  _CreatePipe ::
    Prism' a ()
  _CreatePipe =
    _StdStream . _CreatePipe
  _NoStream ::
    Prism' a ()
  _NoStream =
    _StdStream . _NoStream

instance AsStdStream StdStream where
  _StdStream =
    id
  _Inherit =
    prism'
      (\() -> Inherit)
      (\case
        Inherit -> Just ()
        _ -> Nothing
      )
  _CreatePipe =
    prism'
      (\() -> CreatePipe)
      (\case
        CreatePipe -> Just ()
        _ -> Nothing
      )
  _NoStream =
    prism'
      (\() -> NoStream)
      (\case
        NoStream -> Just ()
        _ -> Nothing
      )

class HasStdStream a where
  stdStream ::
    Lens' a StdStream

instance HasStdStream StdStream where
  stdStream =
    id
