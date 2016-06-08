{-|
module      : Data.Conduit.Process.Utils
description : Utilities for running commands in Conduits
copyright   : (c) michael klein, 2016
license     : bsd3
maintainer  : lambdamichael(at)gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Process.Utils where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TBMQueue (newTBMQueueIO)
import Control.Concurrent.STM.TMQueue (newTMQueueIO)
import Control.Exception (Exception(..), SomeException)
import Control.Lens.Operators ((&), (.~), (^.))
import Control.Lens.TH (makeClassy, makeLenses)
import Control.Monad (Functor(..), Monad(..), join, mapM)
import Data.ByteString (ByteString, length, take)
import Data.Conduit (Conduit, Source, (=$=), runConduit, toConsumer, yield)
import Data.Conduit.Internal (zipSources)
import Data.Conduit.Lens ( getterC )
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Conduit.TQueue (sinkTMQueue, sinkTBMQueue, sourceTMQueue, sourceTBMQueue)
import Data.Conduit.Utils (awaitC, fusingC, zipToSourceWith)
import Data.Either.Utils (maybeTupEither)
import Data.Maybe (Maybe(..))
import Data.Maybe.Utils (justIf)
import Prelude (Bool(..), IO, Either(..), Eq(..), Ord(..), Show, String, ($), (.), const, id, uncurry)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess, proc, shell)
import qualified Data.Conduit.Combinators as C (map, mapM)


data ProcessSource = ProcSource { _exitCode :: Source IO ExitCode
                                , _stdout   :: Source IO ByteString
                                , _stderr   :: Source IO ByteString
                                }

makeLenses ''ProcessSource


data ProcessHandler a = ProcHandler { _goodExit   :: ExitCode   -> IO (Maybe SomeException)
                                    , _goodStderr :: ByteString -> IO (Maybe SomeException)
                                    , _handlerC   :: Conduit ByteString IO a
                                    }

makeClassy ''ProcessHandler


data StderrException = StderrEx { _stderrDetails :: ByteString
                                } deriving (Eq, Show)

instance Exception StderrException


-- | `Source` for a single shell command
singleCmdS :: String -> Source IO ProcessSource
singleCmdS cmd = yield cmd =$= shellC =$= processSourceC

singleProcS :: String -> [String] -> Source IO ProcessSource
singleProcS cmd args = yield (cmd, args) =$= procC =$= processSourceC

-- | This is the default process handler. `_goodExit` is `ExitSuccess`,
-- `_goodStderr` is @True@ for at most one character input (to prevent
-- an empty input with a trailing newline from triggering failure), and
-- `_handlerC` is equivalent to @cat@.
defaultCmd :: ProcessHandler ByteString
defaultCmd = ProcHandler { _goodExit   = \x -> return $ justIf (toException               x) $ x /= ExitSuccess
                         , _goodStderr = \x -> return $ justIf (toException . StderrEx $  x) $ length (take 2 x) < 2
                         , _handlerC   = C.map id
                         }

-- | `defaultCmd`, but considers anything passed to @stderr@ to be
-- acceptable
ignoreStderrCmd :: ProcessHandler ByteString
ignoreStderrCmd = defaultCmd & goodStderr .~ (const $ return Nothing)

-- | `Conduit` from `String` commands to `CreateProcess`, using `shell`
shellC :: Monad m => Conduit String m CreateProcess
shellC = C.map shell

-- | `Conduit` from `String` commands and `[String]` arguments to
-- `CreateProcess`, using `proc`
procC :: Monad m => Conduit (String, [String]) m CreateProcess
procC = C.map $ uncurry proc

-- | Create a process, streaming stdin/stderr out in an asynchronous `TMQueue`.
-- The `ExitCode` is also streamed out, but in a bounded `TBQueue`. This allows
-- things like parsers to be run on the input before the process finishes.
-- Note: `TMQueue`s are closable, and should be closed ASAP upon error. See
-- `mkProcessSource` and `processHandlerSource`
processSourceC :: Conduit CreateProcess IO ProcessSource
processSourceC = C.mapM $ \cp -> do
  stdoutQueue   <- newTMQueueIO
  stderrQueue   <- newTMQueueIO
  exitCodeQueue <- newTBMQueueIO 1

  let stdoutSource   = sourceTMQueue  stdoutQueue
  let stderrSource   = sourceTMQueue  stderrQueue
  let exitCodeSource = sourceTBMQueue exitCodeQueue

  let stdinC    = yield ""
  let stdoutC   = toConsumer $ sinkTMQueue  stdoutQueue   True  -- `True` -> close queue when sink is closed
  let stderrC   = toConsumer $ sinkTMQueue  stderrQueue   True
  let exitCodeC =              sinkTBMQueue exitCodeQueue True

  ~(e, _, _) <- sourceProcessWithStreams cp stdinC stdoutC stderrC
  runConduit $ yield e =$= exitCodeC
  return $ ProcSource { _exitCode = exitCodeSource
                   , _stdout   = stdoutSource
                   , _stderr   = stderrSource
                   }


-- | A `Conduit` that gets the `ExitCode` `Source` from a `ProcessSource`
getProcessExitCode :: Conduit ProcessSource IO (Source IO ExitCode)
getProcessExitCode = getterC exitCode

getProcessStderr :: Conduit ProcessSource IO (Source IO ByteString)
getProcessStderr = getterC stderr

getProcessStdout :: Conduit ProcessSource IO (Source IO ByteString)
getProcessStdout = getterC stdout

-- | Given something with a process handler, check each `ExitCode` as it goes
-- through the `Conduit`
checkExitCode :: HasProcessHandler s a => s -> Conduit (Maybe ExitCode) IO (Maybe SomeException)
checkExitCode ph = C.mapM $ fmap join . mapM (ph ^. goodExit)

checkStderr :: HasProcessHandler s a => s -> Conduit (Maybe ByteString) IO (Maybe SomeException)
checkStderr ph = C.mapM $ fmap join . mapM (ph ^. goodStderr)

goodExitC :: HasProcessHandler s a => s -> Conduit ProcessSource IO (Maybe SomeException)
goodExitC ph = getProcessExitCode =$= awaitC =$= checkExitCode ph

goodStderrC :: HasProcessHandler s a => s -> Conduit ProcessSource IO (Maybe SomeException)
goodStderrC ph = getProcessStderr =$= awaitC =$= checkStderr ph

-- | Forces `goodExit`, `goodStderr` to run in lockstep. If there are errors
-- from both, returns the `goodExit` error first. (`goodExit` is most likely
-- faster, and so is checked first)
goodProcessS :: HasProcessHandler p a => p  -> Source IO ProcessSource -> Source IO (Maybe SomeException)
goodProcessS ph s = zipToSourceWith (<|>) s (goodExitC ph) (goodStderrC ph)

-- | A `Conduit` with `ProcessSource` as input that applies `_handlerC`
processHandlerC :: HasProcessHandler p a => p -> Conduit ProcessSource IO (Source IO a)
processHandlerC ph = getProcessStdout =$= fusingC (ph ^. handlerC)

-- | Use a `ProcessHandler` to convert a `Source` of `ProcessSource`s into
-- `Source` of `Either` `SomeException` or the resulting `Source`.
processHandlerS :: HasProcessHandler p a => p -> Source IO ProcessSource -> Source IO (Either SomeException (Source IO a))
processHandlerS ph s = zipSources (goodProcessS ph s) (s =$= processHandlerC ph) =$= C.map maybeTupEither

