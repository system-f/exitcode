{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.Process(
  module Process
, readCreateProcessWithExitCode
, readProcessWithExitCode
, waitForProcess
, getProcessExitCode
) where

import Control.Category ( Category((.)) )
import Control.Exitcode
    ( Exitcode0, ExitcodeT0, fromExitCode, fromExitCode' )
import Control.Lens ( over, Field1(_1) )
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

readCreateProcessWithExitCode ::
  CreateProcess
  -> String
  -> IO (Exitcode0, String, String)
readCreateProcessWithExitCode p a =
   fmap (over _1 fromExitCode') (P.readCreateProcessWithExitCode p a)

readProcessWithExitCode ::
  FilePath
  -> [String]
  -> String
  -> IO (Exitcode0, String, String)
readProcessWithExitCode p a i =
   fmap (over _1 fromExitCode') (P.readProcessWithExitCode p a i)

waitForProcess ::
  ProcessHandle
  -> IO Exitcode0
waitForProcess =
  fmap fromExitCode' . P.waitForProcess

getProcessExitCode ::
  ProcessHandle
  -> IO (ExitcodeT0 Maybe)
getProcessExitCode =
  fmap fromExitCode . P.getProcessExitCode
