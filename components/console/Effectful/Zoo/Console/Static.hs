module Effectful.Zoo.Console.Static
  ( Console,
    runConsole,
    runConsoleTextToHandle,
    runConsoleTextToStdout,
    runConsoleTextToStderr,
    withConsole,
    print,
    local,
  ) where

import Data.Text.IO qualified as T
import Data.Kind
import Effectful
import Effectful.Zoo.Console.Data.Writer
import Effectful.Zoo.Core
import Effectful.Dispatch.Static
import HaskellWorks.Prelude
import System.IO qualified as IO

data Console (i :: Type) :: Effect

type instance DispatchOf (Console i) = Static NoSideEffects

newtype instance StaticRep (Console i) = Console (Writer i)

runConsole :: ()
  => r <: IOE
  => HasCallStack
  => UnliftStrategy
  -> (HasCallStack => i -> Eff r ())
  -> Eff (Console i : r) a
  -> Eff r a
runConsole strategy run f = do
  s <- mkWriter strategy run
  evalStaticRep (Console s) f

runConsoleTextToHandle :: ()
  => HasCallStack
  => Handle
  -> Eff (Console Text : r) a
  -> Eff r a
runConsoleTextToHandle h =
  evalStaticRep $ Console $ Writer $ T.hPutStrLn h

runConsoleTextToStdout :: ()
  => HasCallStack
  => Eff (Console Text : r) a
  -> Eff r a
runConsoleTextToStdout =
  runConsoleTextToHandle IO.stdout

runConsoleTextToStderr :: ()
  => HasCallStack
  => Eff (Console Text : r) a
  -> Eff r a
runConsoleTextToStderr =
  runConsoleTextToHandle IO.stderr

withConsoleSerialiser :: ()
  => HasCallStack
  => (Writer i -> Writer o)
  -> Eff (Console o : r) a
  -> Eff (Console i : r) a
withConsoleSerialiser f m = do
  writer <- getWriter
  let _ = writer
  raise $ evalStaticRep (Console (f writer)) m

withConsole :: ()
  => HasCallStack
  => (o -> i)
  -> Eff (Console o : r) a
  -> Eff (Console i : r) a
withConsole =
  withConsoleSerialiser . contramap

getWriter :: ()
  => HasCallStack
  => r <: Console i
  => Eff r (Writer i)
getWriter = do
  Console i <- getStaticRep
  pure i

print :: ()
  => HasCallStack
  => r <: Console i
  => r <: IOE
  => i
  -> Eff r ()
print i = do
  writer <- getWriter
  liftIO $ writer.run i

local :: ()
  => HasCallStack
  => r <: Console i
  => (i -> i)
  -> Eff r a
  -> Eff r a
local f =
  localStaticRep $ \(Console s) -> Console (contramap f s)
