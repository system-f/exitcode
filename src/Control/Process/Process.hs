{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.Process(
  module Process
, readCreateProcessWithExitCode
, readProcessWithExitCode
, waitForProcess
, getProcessExitCode
) where

import Control.Applicative ( Applicative(pure) )
import Control.Category ( Category((.)) )
import Control.Exitcode
    ( ExitcodeT0,
      fromExitCode',
      liftExitcode,
      ExitcodeT1,
      _Exitcode1,
      hoistExitcode )
import Control.Lens ( Identity(runIdentity), set )
import Control.Monad ( Monad((>>=)) )
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
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )

readCreateProcessWithExitCode ::
  CreateProcess
  -> String
  -> ExitcodeT1 IO (String, String)
readCreateProcessWithExitCode p a =
  liftExitcode (P.readCreateProcessWithExitCode p a) >>= \(x, y, z) ->
    hoistExitcode (pure . runIdentity) (set _Exitcode1 (y, z) (fromExitCode' x))

readProcessWithExitCode ::
  FilePath
  -> [String]
  -> String
  -> ExitcodeT1 IO (String, String)
readProcessWithExitCode p a i =
  liftExitcode (P.readProcessWithExitCode p a i) >>= \(x, y, z) ->
    hoistExitcode (pure . runIdentity) (set _Exitcode1 (y, z) (fromExitCode' x))

waitForProcess ::
  ProcessHandle
  -> ExitcodeT0 IO
waitForProcess h =
  liftExitcode (P.waitForProcess h) >>= \x ->
    hoistExitcode (pure . runIdentity) (fromExitCode' x)

getProcessExitCode ::
  ProcessHandle
  -> ExitcodeT0 (MaybeT IO)
getProcessExitCode h =
  liftExitcode (MaybeT (P.getProcessExitCode h)) >>=
    hoistExitcode (pure . runIdentity) . fromExitCode'
