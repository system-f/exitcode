{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Control.Process.CmdSpec(
  AsCmdSpec(..)
, HasCmdSpec(..)
) where

import Control.Category ( Category(id, (.)) )
import Control.Lens
    ( Traversable(traverse),
      prism',
      Field1(_1),
      Field2(_2),
      Lens',
      Prism',
      Traversal' )
import Data.Maybe ( Maybe(Nothing, Just) )
import Data.Functor ( Functor(fmap) )
import Data.String ( String )
import Data.Tuple ( uncurry )
import System.FilePath ( FilePath )
import System.Process ( CreateProcess(..), CmdSpec(..) )

class AsCmdSpec a where
  _CmdSpec ::
    Prism' a CmdSpec
  _ShellCommand ::
    Prism' a String
  _ShellCommand =
    _CmdSpec . _ShellCommand
  _RawCommand ::
    Prism' a (FilePath, [String])
  _RawCommand =
    _CmdSpec . _RawCommand
  _RawCommandExe ::
    Traversal' a FilePath
  _RawCommandExe =
    _RawCommand . _1
  _RawCommandArgumentList ::
    Traversal' a [String]
  _RawCommandArgumentList =
    _RawCommand . _2
  _RawCommandArguments ::
    Traversal' a String
  _RawCommandArguments =
    _RawCommandArgumentList . traverse

instance AsCmdSpec CmdSpec where
  _CmdSpec = id
  _ShellCommand =
    prism'
      ShellCommand
      (\case
        ShellCommand a -> Just a
        _ -> Nothing
      )
  _RawCommand =
    prism'
      (uncurry RawCommand)
      (\case
        RawCommand a b -> Just (a, b)
        _ -> Nothing
      )

class HasCmdSpec a where
  cmdSpec ::
    Lens' a CmdSpec
  shellCommand ::
    Traversal' a String
  shellCommand =
    cmdSpec . _ShellCommand
  rawCommand ::
    Traversal' a (FilePath, [String])
  rawCommand =
    cmdSpec . _RawCommand
  rawCommandExe ::
    Traversal' a FilePath
  rawCommandExe =
    rawCommand . _1
  rawCommandArgumentList ::
    Traversal' a [String]
  rawCommandArgumentList =
    rawCommand . _2
  rawCommandArguments ::
    Traversal' a String
  rawCommandArguments =
    rawCommandArgumentList . traverse

instance HasCmdSpec CmdSpec where
  cmdSpec =
    id

instance HasCmdSpec CreateProcess where
  cmdSpec f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\csc' -> CreateProcess csc' cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) (f csc)
