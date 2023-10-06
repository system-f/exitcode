{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Process.Process(
  module Process
, readCreateProcessWithExitCode
, readProcessWithExitCode
, waitForProcess
, getProcessExitCode
, getProcessExitCodeBool
) where

import Control.Applicative ( Applicative(pure) )
import Control.Category ( Category((.)) )
import Control.Exception ( Exception )
import Control.Exitcode
    ( ExitcodeT,
      fromExitCode',
      liftExitcode,
      hoistExitcode,
      tryExitcode,
      _Exitcode1,
      liftTryExitcode )
import Control.Lens ( Identity(runIdentity), set )
import Control.Monad ( Monad((>>=)) )
import Control.Monad.Except ( ExceptT(..) )
import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Bool ( Bool )
import Data.Maybe ( Maybe(..), isJust, maybe )
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
  Exception e' =>
  CreateProcess
  -> String
  -> ExitcodeT (ExceptT e' IO) (String, String) (String, String)
readCreateProcessWithExitCode p a =
  tryExitcode (liftExitcode (P.readCreateProcessWithExitCode p a)) >>= \(x, y, z) ->
    hoistExitcode (pure . runIdentity) (set _Exitcode1 (y, z) (fromExitCode' x))

readProcessWithExitCode ::
  Exception e' =>
  FilePath
  -> [String]
  -> String
  -> ExitcodeT (ExceptT e' IO) (String, String) (String, String)
readProcessWithExitCode p a i =
  tryExitcode (liftExitcode (P.readProcessWithExitCode p a i)) >>= \(x, y, z) ->
    hoistExitcode (pure . runIdentity) (set _Exitcode1 (y, z) (fromExitCode' x))

waitForProcess ::
  Exception e' =>
  ProcessHandle
  -> ExitcodeT (ExceptT e' IO) () ()
waitForProcess h =
  tryExitcode (liftExitcode (P.waitForProcess h)) >>= \x ->
    hoistExitcode (pure . runIdentity) (fromExitCode' x)

getProcessExitCode ::
  Exception e' =>
  ProcessHandle
  -> ExitcodeT (ExceptT e' IO) (Maybe ()) (Maybe ())
getProcessExitCode h =
  liftTryExitcode (P.getProcessExitCode h) >>=
    maybe (pure Nothing) (hoistExitcode (pure . runIdentity) . set _Exitcode1 (Just ()) . fromExitCode')

getProcessExitCodeBool ::
  Exception e' =>
  ProcessHandle
  -> ExitcodeT (ExceptT e' IO) Bool Bool
getProcessExitCodeBool =
  bimap isJust isJust . getProcessExitCode
