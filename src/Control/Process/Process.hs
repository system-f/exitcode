{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.Process(
  module Process
, readCreateProcessWithExitCode
, readCreateProcessWithExitCode'
, readProcessWithExitCode
, readProcessWithExitCode'
, waitForProcess
, waitForProcess'
, getProcessExitCode
, getProcessExitCode'
) where

import Control.Category ( Category((.)) )
import Control.Exitcode
    ( ExitcodeT,
      fromExitCode,
      fromExitCode',
      _ExitSuccess,
      liftIso,
      ExitcodeT0',
      Exitcode0',
      _ExitFailureE )
import Control.Lens ( over, view, review, set, Field1(_1) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Data.Functor ( Functor(fmap) )
import Data.Maybe ( Maybe )
import Data.String ( String )
import System.FilePath( FilePath )
import System.IO ( IO )
import System.Process as Process(
    createProcess
  , createProcess_
  , shell
  , proc
  , CreateProcess()
  , CmdSpec(..)
  , StdStream(..)
  , ProcessHandle
  , callProcess
  , callCommand
  , spawnProcess
  , readCreateProcess
  , readProcess
  , withCreateProcess
  , cleanupProcess
  , showCommandForUser
  , Pid
  , getPid
  , getCurrentPid
  , terminateProcess
  , interruptProcessGroupOf
  , createPipe
  , createPipeFd
  )
import qualified System.Process as P(readCreateProcessWithExitCode, readProcessWithExitCode, waitForProcess, getProcessExitCode)

readCreateProcessWithExitCode' ::
  CreateProcess
  -> String
  -> ExitcodeT String IO String
readCreateProcessWithExitCode' p a =
  review liftIso (fmap (\(x, y, z) -> set _ExitSuccess y (set _ExitFailureE z x)) (readCreateProcessWithExitCode p a))

readCreateProcessWithExitCode ::
  CreateProcess
  -> String
  -> IO (Exitcode0', String, String)
readCreateProcessWithExitCode p a =
  fmap (over _1 fromExitCode') (P.readCreateProcessWithExitCode p a)

readProcessWithExitCode' ::
  FilePath
  -> [String]
  -> String
  -> ExitcodeT String IO String
readProcessWithExitCode' p a i =
  review liftIso (fmap (\(x, y, z) -> set _ExitSuccess y (set _ExitFailureE z x)) (readProcessWithExitCode p a i))

readProcessWithExitCode ::
  FilePath
  -> [String]
  -> String
  -> IO (Exitcode0', String, String)
readProcessWithExitCode p a i =
  fmap (over _1 fromExitCode') (P.readProcessWithExitCode p a i)

waitForProcess ::
  ProcessHandle
  -> IO Exitcode0'
waitForProcess =
  fmap fromExitCode' . P.waitForProcess

waitForProcess' ::
  ProcessHandle
  -> ExitcodeT0' IO
waitForProcess' =
  review liftIso . waitForProcess

getProcessExitCode ::
  ProcessHandle
  -> IO (ExitcodeT0' Maybe)
getProcessExitCode =
  fmap fromExitCode . P.getProcessExitCode

getProcessExitCode' ::
  ProcessHandle
  -> ExitcodeT0' (MaybeT IO)
getProcessExitCode' =
  review liftIso . MaybeT . fmap (view liftIso) . getProcessExitCode
