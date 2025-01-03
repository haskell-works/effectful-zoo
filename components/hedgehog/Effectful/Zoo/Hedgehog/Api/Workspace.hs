module Effectful.Zoo.Hedgehog.Api.Workspace
  ( PackagePath(..),
    ProjectRoot(..),
    Workspace(..),
    workspace,
    moduleWorkspace,
    findCabalProjectDir,
  ) where

import Data.Text qualified as T
import Effectful
import Effectful.Environment
import Effectful.Reader.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.FileSystem
import Effectful.Zoo.Hedgehog.Api.Assert
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Api.Stack
import Effectful.Zoo.Hedgehog.Data.PackagePath
import Effectful.Zoo.Hedgehog.Data.ProjectRoot
import Effectful.Zoo.Hedgehog.Data.Workspace
import Effectful.Zoo.Hedgehog.Dynamic
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude
import System.FilePath ((</>))
import System.Info

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the supplied prefix but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
workspace :: ()
  => HasCallStack
  => r <: Environment
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => FilePath
  -> Eff (Reader Workspace : r) ()
  -> Eff r ()
workspace prefixPath f = withFrozenCallStack $ do
  systemTemp <- getCanonicalTemporaryDirectory
    & trapFail @IOException
  maybeKeepWorkspace <- lookupEnv "KEEP_WORKSPACE"
  ws <- createTempDirectory systemTemp (prefixPath <> "-test")
    & trapFail @IOException
  jot_ $ "Workspace: " <> T.pack ws
  writeStringFile (ws </> "module") callerModuleName
    & trapFail @IOException
  runReader (Workspace ws) f
  when (os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    removePathForcibly ws
      & trapFail @IOException

-- | Create a workspace directory which will exist for at least the duration of
-- the supplied block.
--
-- The directory will have the prefix as "$prefixPath/$moduleName" but contain a generated random
-- suffix to prevent interference between tests
--
-- The directory will be deleted if the block succeeds, but left behind if
-- the block fails.
--
-- The 'prefix' argument should not contain directory delimeters.
moduleWorkspace ::  ()
  => HasCallStack
  => r <: Environment
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => String
  -> Eff (Reader Workspace : r) ()
  -> Eff r ()
moduleWorkspace prefix f = withFrozenCallStack $
  workspace (prefix <> "-" <> callerModuleName) f

-- | Compute the project base.  This will be the first parent directory that contains
-- the `cabal.project` file.
-- This should should point to the root directory of the Github project checkout.
findCabalProjectDir :: ()
  => r <: Environment
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => FilePath
  -> Eff r FilePath
findCabalProjectDir dir = do
  atBase <- doesFileExist (dir </> "cabal.project")
    & trap_ @IOException (pure False)
  if atBase
    then return dir
    else do
      let up = dir </> ".."
      upExist <- doesDirectoryExist up
        & trap_ @IOException (pure False)
      if upExist
        then findCabalProjectDir up
        else failWith Nothing "Could not detect project base directory (containing cabal.project)"
