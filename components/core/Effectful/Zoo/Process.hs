module Effectful.Zoo.Process
  ( IO.CreateProcess(..),
    IO.CmdSpec(..),
    IO.StdStream(..),
    Handle,
    ProcessHandle,
    ExitCode(..),
    FD,
    Pid,
    createProcess,
    createProcess_,
    IO.shell,
    IO.proc,
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
    cleanupProcess,
    getPid,
    getCurrentPid,
    interruptProcessGroupOf,
    createPipe,
    createPipeFd,
    runProcess,
    runCommand,
    runInteractiveProcess,
    runInteractiveCommand,
    system,
    rawSystem,

    waitSecondsForProcess,
  ) where

import Control.Exception qualified as CE
import HaskellWorks.Error
import HaskellWorks.Error.Types
import HaskellWorks.IO.Process qualified as IO
import HaskellWorks.Prelude
import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import System.Exit (ExitCode (..))
import System.Posix.Internals (FD)
import System.Process (Pid, ProcessHandle)
import System.Process qualified as IO

createProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => IO.CreateProcess
  -> Eff r (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  r <- liftIO $ CE.try @IOException $ IO.createProcess cp
  fromEither r

createProcess_ :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> IO.CreateProcess
  -> Eff r (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ cmd cp = do
  r <- liftIO $ CE.try @IOException $ IO.createProcess_ cmd cp
  fromEither r

callProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> [String]
  -> Eff r ()
callProcess cmd args = do
  r <- liftIO $ CE.try @IOException $ IO.callProcess cmd args
  fromEither r

callCommand :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> Eff r ()
callCommand cmd = do
  r <- liftIO $ CE.try @IOException $ IO.callCommand cmd
  fromEither r

spawnProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> [String]
  -> Eff r ProcessHandle
spawnProcess cmd args = do
  r <- liftIO $ CE.try @IOException $ IO.spawnProcess cmd args
  fromEither r

spawnCommand :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> Eff r ProcessHandle
spawnCommand cmd = do
  r <- liftIO $ CE.try @IOException $ IO.spawnCommand cmd
  fromEither r

readCreateProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => IO.CreateProcess
  -> String
  -> Eff r String
readCreateProcess cp input = do
  r <- liftIO $ CE.try @IOException $ IO.readCreateProcess cp input
  fromEither r

readProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> [String]
  -> String
  -> Eff r String
readProcess cmd args input = do
  r <- liftIO $ CE.try @IOException $ IO.readProcess cmd args input
  fromEither r

readCreateProcessWithExitCode :: ()
  => r <: Error IOException
  => r <: IOE
  => IO.CreateProcess
  -> String
  -> Eff r (ExitCode, String, String)
readCreateProcessWithExitCode cp input = do
  r <- liftIO $ CE.try @IOException $ IO.readCreateProcessWithExitCode cp input
  fromEither r

readProcessWithExitCode :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> [String]
  -> String
  -> Eff r (ExitCode, String, String)
readProcessWithExitCode cmd args input = do
  r <- liftIO $ CE.try @IOException $ IO.readProcessWithExitCode cmd args input
  fromEither r

cleanupProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> Eff r ()
cleanupProcess (mIn, mOut, mErr, ph) = do
  r <- liftIO $ CE.try @IOException $ IO.cleanupProcess (mIn, mOut, mErr, ph)
  fromEither r

getPid :: ()
  => r <: Error IOException
  => r <: IOE
  => ProcessHandle
  -> Eff r (Maybe Pid)
getPid ph = do
  r <- liftIO $ CE.try @IOException $ IO.getPid ph
  fromEither r

getCurrentPid :: ()
  => r <: Error IOException
  => r <: IOE
  => Eff r Pid
getCurrentPid = do
  r <- liftIO $ CE.try @IOException $ IO.getCurrentPid
  fromEither r

interruptProcessGroupOf :: ()
  => r <: Error IOException
  => r <: IOE
  => ProcessHandle
  -> Eff r ()
interruptProcessGroupOf ph = do
  r <- liftIO $ CE.try @IOException $ IO.interruptProcessGroupOf ph
  fromEither r

createPipe :: ()
  => r <: Error IOException
  => r <: IOE
  => Eff r (Handle, Handle)
createPipe = do
  r <- liftIO $ CE.try @IOException $ IO.createPipe
  fromEither r

createPipeFd :: ()
  => r <: Error IOException
  => r <: IOE
  => Eff r (FD, FD)
createPipeFd = do
  r <- liftIO $ CE.try @IOException $ IO.createPipeFd
  fromEither r

runProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> Eff r ProcessHandle
runProcess cmd args mbStdIn mbEnv mbCwd mbStdOut mbStdErr = do
  r <- liftIO $ CE.try @IOException $ IO.runProcess cmd args mbStdIn mbEnv mbCwd mbStdOut mbStdErr
  fromEither r

runCommand :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> Eff r ProcessHandle
runCommand cmd = do
  r <- liftIO $ CE.try @IOException $ IO.runCommand cmd
  fromEither r

runInteractiveProcess :: ()
  => r <: Error IOException
  => r <: IOE
  => FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> Eff r (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess cmd args mbCwd mbEnv = do
  r <- liftIO $ CE.try @IOException $ IO.runInteractiveProcess cmd args mbCwd mbEnv
  fromEither r

runInteractiveCommand :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> Eff r (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommand cmd = do
  r <- liftIO $ CE.try @IOException $ IO.runInteractiveCommand cmd
  fromEither r

system :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> Eff r ExitCode
system cmd = do
  r <- liftIO $ CE.try @IOException $ IO.system cmd
  fromEither r

rawSystem :: ()
  => r <: Error IOException
  => r <: IOE
  => String
  -> [String]
  -> Eff r ExitCode
rawSystem cmd args = do
  r <- liftIO $ CE.try @IOException $ IO.rawSystem cmd args
  fromEither r

waitSecondsForProcess :: ()
  => r <: Error TimedOut
  => r <: IOE
  => Int
  -> ProcessHandle
  -> Eff r (Maybe ExitCode)
waitSecondsForProcess seconds hProcess =
  liftIO (IO.waitSecondsForProcess seconds hProcess)
    & onLeftM @TimedOut throw
