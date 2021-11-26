{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.CreateProcess(
  HasCreateProcess(..)
, AsCreateProcess(..)
, streams
, streams1
) where

import Control.Applicative ( Applicative((<*>)) )
import Control.Category ( Category(id, (.)) )
import Control.Lens
    ( Traversable(traverse),
      _Just,
      only,
      Field1(_1),
      Field2(_2),
      Lens',
      Prism',
      Traversal',
      Traversal1' )
import Control.Process.UserID ( HasUserID(userIDWord32) )
import Data.Bool ( Bool(True) )
import Data.Functor ( Functor(fmap), (<$>) )
import Data.Functor.Apply ( Apply((<.>)) )
import Data.Maybe ( Maybe(..) )
import Data.String ( String )
import Data.Word ( Word32 )
import System.FilePath ( FilePath )
import System.Process(StdStream(..), CreateProcess(CreateProcess))
import System.Process.Internals(GroupID, UserID)

class HasCreateProcess a where
  createProcess ::
    Lens' a CreateProcess
  child_group ::
    Lens' a (Maybe GroupID)
  {-# INLINE child_group #-}
  child_group =
    createProcess . child_group
  child_user ::
    Lens' a (Maybe UserID)
  {-# INLINE child_user #-}
  child_user =
    createProcess . child_user
  close_fds ::
    Lens' a Bool
  {-# INLINE close_fds #-}
  close_fds =
    createProcess . close_fds
  create_group ::
    Lens' a Bool
  {-# INLINE create_group #-}
  create_group =
    createProcess . create_group
  create_new_console ::
    Lens' a Bool
  {-# INLINE create_new_console #-}
  create_new_console =
    createProcess . create_new_console
  cwd ::
    Lens' a (Maybe FilePath)
  {-# INLINE cwd #-}
  cwd =
    createProcess . cwd
  delegate_ctlc ::
    Lens' a Bool
  {-# INLINE delegate_ctlc #-}
  delegate_ctlc =
    createProcess . delegate_ctlc
  detach_console ::
    Lens' a Bool
  {-# INLINE detach_console #-}
  detach_console =
    createProcess . detach_console
  env ::
    Lens' a (Maybe [(String, String)])
  {-# INLINE env #-}
  env =
    createProcess . env
  new_session ::
    Lens' a Bool
  {-# INLINE new_session #-}
  new_session =
    createProcess . new_session
  std_err ::
    Lens' a StdStream
  {-# INLINE std_err #-}
  std_err =
    createProcess . std_err
  std_in ::
    Lens' a StdStream
  {-# INLINE std_in #-}
  std_in =
    createProcess . std_in
  std_out ::
    Lens' a StdStream
  {-# INLINE std_out #-}
  std_out =
    createProcess . std_out
  use_process_jobs ::
    Lens' a Bool
  {-# INLINE use_process_jobs #-}
  use_process_jobs =
    createProcess . use_process_jobs

  cwd' ::
    Traversal' a FilePath
  cwd' =
    cwd . _Just
  envList ::
    Traversal' a [(String, String)]
  envList =
    env . _Just
  envElement ::
    Traversal' a (String, String)
  envElement =
    envList . traverse
  envElementKey ::
    Traversal' a String
  envElementKey =
    envElement . _1
  envElementValue ::
    Traversal' a String
  envElementValue =
    envElement . _2
  close_fds' ::
    Traversal' a ()
  close_fds' =
    close_fds . only True
  create_group' ::
    Traversal' a ()
  create_group' =
    create_group . only True
  delegate_ctlc' ::
    Traversal' a ()
  delegate_ctlc' =
    delegate_ctlc . only True
  detach_console' ::
    Traversal' a ()
  detach_console' =
    detach_console . only True
  create_new_console' ::
    Traversal' a ()
  create_new_console' =
    create_new_console . only True
  new_session' ::
    Traversal' a ()
  new_session' =
    new_session . only True
  child_group' ::
    Traversal' a GroupID
  child_group' =
    child_group . _Just
  child_user' ::
    Traversal' a UserID
  child_user' =
    child_user . _Just
  child_user'' ::
    Traversal' a Word32
  child_user'' =
    child_user' . userIDWord32
  use_process_jobs' ::
    Traversal' a ()
  use_process_jobs' =
    use_process_jobs . only True

instance HasCreateProcess CreateProcess where
  createProcess =
    id
  child_group f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\chg' -> CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg' chu upj) (f chg)
  child_user f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\chu' -> CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu' upj) (f chu)
  close_fds f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\clf' -> CreateProcess csc cw en sti sto ste clf' crg dct dcl cnc nss chg chu upj) (f clf)
  create_group f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\crg' -> CreateProcess csc cw en sti sto ste clf crg' dct dcl cnc nss chg chu upj) (f crg)
  create_new_console f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\cnc' -> CreateProcess csc cw en sti sto ste clf crg dct dcl cnc' nss chg chu upj) (f cnc)
  cwd f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\cw' -> CreateProcess csc cw' en sti sto ste clf crg dct dcl cnc nss chg chu upj) (f cw)
  delegate_ctlc f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\dct' -> CreateProcess csc cw en sti sto ste clf crg dct' dcl cnc nss chg chu upj) (f dct)
  detach_console f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\dcl' -> CreateProcess csc cw en sti sto ste clf crg dct dcl' cnc nss chg chu upj) (f dcl)
  env f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\en' -> CreateProcess csc cw en' sti sto ste clf crg dct dcl cnc nss chg chu upj) (f en)
  new_session f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\nss' -> CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss' chg chu upj) (f nss)
  std_err f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\ste' -> CreateProcess csc cw en sti sto ste' clf crg dct dcl cnc nss chg chu upj) (f ste)
  std_in f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\sti' -> CreateProcess csc cw en sti' sto ste clf crg dct dcl cnc nss chg chu upj) (f sti)
  std_out f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\sto' -> CreateProcess csc cw en sti sto' ste clf crg dct dcl cnc nss chg chu upj) (f sto)
  use_process_jobs f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
    fmap (\upj' -> CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj') (f upj)

class AsCreateProcess a where
  _CreateProcess ::
    Prism' a CreateProcess

instance AsCreateProcess CreateProcess where
  _CreateProcess =
    id

streams ::
  Traversal' CreateProcess StdStream
streams f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
  (\sti' sto' ste' -> CreateProcess csc cw en sti' sto' ste' clf crg dct dcl cnc nss chg chu upj) <$> f sti <*> f sto <*> f ste

streams1 ::
  Traversal1' CreateProcess StdStream
streams1 f (CreateProcess csc cw en sti sto ste clf crg dct dcl cnc nss chg chu upj) =
  (\sti' sto' ste' -> CreateProcess csc cw en sti' sto' ste' clf crg dct dcl cnc nss chg chu upj) <$> f sti <.> f sto <.> f ste
