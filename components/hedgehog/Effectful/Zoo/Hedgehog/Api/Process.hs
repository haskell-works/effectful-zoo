module Effectful.Zoo.Hedgehog.Api.Process
  ( ExecConfig(..),
    defaultExecConfig,
    execDetailFlex,
    execFlex,
    execFlexOk,
    execFlexOk',
    execOk,
    execOk_,
    exec,
    procFlex,
    procFlex',
    binFlex,

    waitSecondsForProcess,
    waitSecondsForProcessOk,

  ) where

import Data.List qualified as L
import Data.Monoid (Last (..))
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Zoo.Core
import Effectful.Zoo.Environment
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.Assert
import Effectful.Zoo.Hedgehog.Api.Internal.Cabal
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Api.Process.Internal
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.Process
import GHC.Stack (callStack)
import HaskellWorks.Error.Types
import HaskellWorks.Prelude
import Effectful.FileSystem (FileSystem)

-- | Configuration for starting a new process.  This is a subset of 'IO.CreateProcess'.
data ExecConfig = ExecConfig
  { execConfigEnv :: Last [(String, String)]
  , execConfigCwd :: Last FilePath
  } deriving stock (Eq, Generic, Show)

defaultExecConfig :: ExecConfig
defaultExecConfig = ExecConfig
  { execConfigEnv = mempty
  , execConfigCwd = mempty
  }

-- | Create a process returning its stdout.
--
-- Being a 'flex' function means that the environment determines how the process is launched.
--
-- When running in a nix environment, the 'envBin' argument describes the environment variable
-- that defines the binary to use to launch the process.
--
-- When running outside a nix environment, the `pkgBin` describes the name of the binary
-- to launch via cabal exec.
execFlexOk :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => String
  -> String
  -> [String]
  -> Eff r String
execFlexOk = execFlexOk' defaultExecConfig

execFlexOk' :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> Eff r String
execFlexOk' execConfig pkgBin envBin arguments =
  withFrozenCallStack do
    (exitResult, stdout, stderr) <- execFlex execConfig pkgBin envBin arguments
    case exitResult of
      ExitFailure exitCode -> do
        jotString_ $ L.unlines $
          [ "Process exited with non-zero exit-code: " <> show @Int exitCode ]
          <> (if L.null stdout then [] else ["━━━━ stdout ━━━━" , stdout])
          <> (if L.null stderr then [] else ["━━━━ stderr ━━━━" , stderr])
        failMessage callStack "Execute process failed"
      ExitSuccess -> return stdout

-- | Run a process, returning its exit code, its stdout, and its stderr.
-- Contrary to @execFlexOk'@, this function doesn't fail if the call fails.
-- So, if you want to test something negative, this is the function to use.
execFlex :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => ExecConfig
  -> String -- ^ @pkgBin@: name of the binary to launch via 'cabal exec'
  -> String -- ^ @envBin@: environment variable defining the binary to launch the process, when in Nix
  -> [String]
  -> Eff r (ExitCode, String, String) -- ^ exit code, stdout, stderr
execFlex execConfig pkgBin envBin arguments =
  withFrozenCallStack do
    cp <- procFlex' execConfig pkgBin envBin arguments
    jotString_ . ("━━━━ command ━━━━\n" <>) $ case cmdspec cp of
      ShellCommand cmd    -> cmd
      RawCommand cmd args -> cmd <> " " <> L.unwords (argQuote <$> args)

    readCreateProcessWithExitCode cp ""

execDetailFlex :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => ExecConfig
  -> String
  -> String
  -> [String]
  -> Eff r (ExitCode, String, String)
execDetailFlex execConfig pkgBin envBin arguments =
  withFrozenCallStack do
    cp <- procFlex' execConfig pkgBin envBin arguments
    jotString_ . ("Command: " <>) $ case cmdspec cp of
      ShellCommand cmd    -> cmd
      RawCommand cmd args -> cmd <> " " <> L.unwords args
    readCreateProcessWithExitCode cp ""

-- | Execute a process, returning '()'.
execOk_ :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: Hedgehog
  => r <: IOE
  => ExecConfig
  -> String
  -> [String]
  -> Eff r ()
execOk_ execConfig bin arguments =
  void $ execOk execConfig bin arguments

-- | Execute a process, returning the stdout. Fail if the call returns
-- with a non-zero exit code. For a version that doesn't fail upon receiving
-- a non-zero exit code, see 'execAny'.
execOk :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: Hedgehog
  => r <: IOE
  => ExecConfig
  -> String
  -> [String]
  -> Eff r String
execOk execConfig bin arguments =
  withFrozenCallStack do
    (exitResult, stdout, stderr) <- exec execConfig bin arguments
    case exitResult of
      ExitFailure exitCode ->failMessage callStack . L.unlines $
        [ "Process exited with non-zero exit-code: " <> show @Int exitCode ]
        <> (if L.null stdout then [] else ["━━━━ stdout ━━━━" , stdout])
        <> (if L.null stderr then [] else ["━━━━ stderr ━━━━" , stderr])
      ExitSuccess -> return stdout

-- | Execute a process, returning the error code, the stdout, and the stderr.
exec :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: Hedgehog
  => r <: IOE
  => ExecConfig
  -> String -- ^ The binary to launch
  -> [String] -- ^ The binary's arguments
  -> Eff r (ExitCode, String, String) -- ^ exit code, stdout, stderr
exec execConfig bin arguments =
  withFrozenCallStack do
    let cp = (proc bin arguments)
          { env = getLast execConfig.execConfigEnv
          , cwd = getLast execConfig.execConfigCwd
          }
    jotString_ . ( "━━━━ command ━━━━\n" <>) $ bin <> " " <> L.unwords (argQuote <$> arguments)
    readCreateProcessWithExitCode cp ""

-- | Wait a maximum of 'seconds' secons for process to exit.
waitSecondsForProcessOk :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: IOE
  => Int
  -> ProcessHandle
  -> Eff r ExitCode
waitSecondsForProcessOk seconds hProcess =
  withFrozenCallStack do
    maybeExitCode <- waitSecondsForProcess seconds hProcess
      & trapFail @TimedOut

    case maybeExitCode of
      Nothing -> failMessage callStack "No exit code for process"
      Just exitCode -> do
        jotString_ $ "Process exited " <> show exitCode
        return exitCode

-- | Compute the path to the binary given a package name or an environment variable override.
binFlex :: ()
  => HasCallStack
  => r <: Environment
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: IOE
  => r <: Log Text
  => String
  -- ^ Package name
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> Eff r FilePath
  -- ^ Path to executable
binFlex pkg binaryEnv =
  withFrozenCallStack do
    maybeEnvBin <- lookupEnvMaybe $ T.pack binaryEnv
    case maybeEnvBin of
      Just envBin -> return $ T.unpack envBin
      Nothing     -> binDist pkg

-- | Create a 'CreateProcess' describing how to start a process given the Cabal package name
-- corresponding to the executable, an environment variable pointing to the executable,
-- and an argument list.
--
-- The actual executable used will the one specified by the environment variable, but if
-- the environment variable is not defined, it will be found instead by consulting the
-- "plan.json" generated by cabal.  It is assumed that the project has already been
-- configured and the executable has been built.
procFlex :: ()
  => HasCallStack
  => r <: Environment
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: IOE
  => r <: Log Text
  => String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> Eff r CreateProcess
  -- ^ Captured stdout
procFlex =
  procFlex' defaultExecConfig

procFlex' :: ()
  => HasCallStack
  => r <: Environment
  => r <: Error GenericError
  => r <: Error IOException
  => r <: FileSystem
  => r <: IOE
  => r <: Log Text
  => ExecConfig
  -> String
  -- ^ Cabal package name corresponding to the executable
  -> String
  -- ^ Environment variable pointing to the binary to run
  -> [String]
  -- ^ Arguments to the CLI command
  -> Eff r CreateProcess
  -- ^ Captured stdout
procFlex' execConfig pkg binaryEnv arguments =
  withFrozenCallStack do
    bin <- binFlex pkg binaryEnv
    return (proc bin arguments)
      { env = getLast execConfig.execConfigEnv
      , cwd = getLast execConfig.execConfigCwd
      -- this allows sending signals to the created processes, without killing the test-suite process
      , create_group = True
      }
