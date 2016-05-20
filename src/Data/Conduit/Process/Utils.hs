{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Conduit.Process.Utils where


import qualified Control.Monad as M (liftM, mapM)
-- import Control.Monad.Trans.Class (lift)

import Control.Concurrent.STM.TMQueue (newTMQueueIO)
import Control.Lens.Getter (Getting(..), (^.))
import Control.Lens.TH (makeClassy, makeLenses)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit(..), Source(..), ($$), (=$=), await, fuseBoth)
import Data.Conduit.Combinators (map, mapM)
import Data.Conduit.Internal (zipSources)
import Data.Conduit.TQueue
import Data.Maybe (Maybe(..))
import Prelude (Bool(..), IO(..), (.), ($), (&&), curry, id, uncurry)
import System.Exit (ExitCode (..))
import qualified Control.Monad as M (Monad(..), liftM)
import Data.Traversable (Traversable, traverse)


-- sourceProcessWithStreams
--  :: CreateProcess
--  -> Producer IO ByteString    -- stdin
--  -> Consumer ByteString IO a  -- stdout
--  -> Consumer ByteString IO b  -- stderr
--  -> IO (ExitCode, a, b)

-- fst3 :: (a, b, c) -> a
-- fst3 ~(x, _, _) = x


-- | Create a process, streaming stdin/stderr out in an asynchronous `TMQueue`.
-- The `ExitCode` is also streamed out, but in a bounded `TBQueue`. This allows
-- things like parsers to be run on the input before the process finishes.
-- Note: `TMQueue`s are closable, and should be closed ASAP upon error.


data ProcessSource = ProcSource { _exitCode :: Source IO ExitCode
                                , _stdout   :: Source IO ByteString
                                , _stderr   :: Source IO ByteString
                                }

makeLenses ''ProcessSource

data ProcessHandler a = ProcHandler { _goodExit    :: ExitCode   -> IO Bool
                                    , _goodStderr  :: ByteString -> IO Bool
                                    , _procConduit :: Conduit ByteString IO a
                                    }

makeClassy ''ProcessHandler

-- | This is the default process handler. `_goodExit` is `ExitSuccess`,
-- `_goodStderr` is @True@ for at most one character input (to prevent
-- an empty input with a trailing newline from triggering failure), and
-- `_procConduit` is equivalent to @cat@.
defaultProcessHandler = ProcHandler { _goodExit    = (== ExitSuccess)
                                    , _goodStderr  = (\x -> length (take 2 x) < 2)
                                    , _procConduit = map id
                                    }


processSourceConduit :: Conduit CreateProcess IO ProcessSource


mkProcessSource :: CreateProcess -> IO ProcessSource
mkProcessSource cp = do
  stdoutQueue   <- newTMQueueIO
  stderrQueue   <- newTMQueueIO
  exitCodeQueue <- newTBMQueueIO 1

  stdinProducer = yield ""
  stdoutSource  = sourceTMQueue stdoutQueue
  stderrSource  = sourceTMQueue stderrQueue

  stdoutSink = sinkToConsumer $ sinkTMQueue stdoutQueue True  -- `True` -> close queue when sink is closed
  stderrSink = sinkToConsumer $ sinkTMQueue stderrQueue True

sourceProcessWithStreams :: CreateProcess
                         -> Producer IO ByteString   -- stdin
                         -> Consumer ByteString IO a -- stdout
                         -> Consumer ByteString IO b -- stderr
                         -> IO (ExitCode, a, b)


-- | Connect the `_stdout` to the `_procConduit` and run.
-- If not `_goodExit` and `_goodStdErr`, yield `Nothing`,
-- else yield `Just` the result of the `_procConduit`.
-- This allows `_stdout` to be processed before the process
-- finishes, while still handling process failure.
-- handleProcess :: ProcessHandler a -> Conduit ProcessSource IO (Maybe a)
-- handleProcess ph ps = do
--   results :: Source IO a
--   results <- ps^.stdout   ?? ph^.procConduit
--   gExit   = ps^.exitCode $$ ph^.goodExit
--   gStdErr = ps^.stderr   $$ ph^.goodStdErr

--   successSource :: Source IO Bool
--   successSource = map (uncurry (&&)) $ zipSources gExit gStdErr

--   results1Source :: Source IO (a, Bool)
--   results1Source = zipSources results successSource

--   finalSource :: Source IO (Maybe a)
--   finalSource = map (uncurry justIfTrue) results1Source





-- | Given a getter @g@, @getterConduit g@ takes what @g@ gets from as input and
-- outputs what is gotten by @g@. For example,
-- @getterConduit _1 :: Conduit (a,b) m a@
getterConduit :: M.Monad m => Getting a s a -> Conduit s m a
getterConduit g = map (^. g)

-- | A `Conduit` that gets the `ExitCode` `Source` from a `ProcessSource`
getProcessExitCode :: Conduit ProcessSource IO (Source IO ExitCode)
getProcessExitCode = getterConduit exitCode

getProcessStderr :: Conduit ProcessSource IO (Source IO ByteString)
getProcessStderr = getterConduit stderr

getProcessStdout :: Conduit ProcessSource IO (Source IO ByteString)
getProcessStdout = getterConduit stdout

-- | `await` for a value from each `Source`, outputting `Nothing` when the
-- `Source` is empty
awaitConduit :: M.Monad m => Conduit (Source m a) m (Maybe a)
awaitConduit = mapM ($$ await)

-- | Given something with a process handler, check each `ExitCode` as it goes
-- through the `Conduit`
checkExitCode :: (Traversable t, HasProcessHandler s a) => s -> Conduit (t ExitCode) IO (t Bool)
checkExitCode ph = mapM (traverse (ph ^. goodExit))

checkStderr :: (Traversable t, HasProcessHandler s a) => s -> Conduit (t ByteString) IO (t Bool)
checkStderr ph = mapM (traverse (ph ^. goodStderr))



-- | A `Conduit` that converts @Maybe Bool@ to @Bool@, taking @Just True@
-- as the only @True@ value
fromMaybeBool :: M.Monad m => Conduit (Maybe Bool) m Bool
fromMaybeBool = map fromMaybeBool'
  where
    fromMaybeBool' :: Maybe Bool -> Bool
    fromMaybeBool' (Just True) = True
    fromMaybeBool'  _          = False

goodExitConduit :: HasProcessHandler s a => s -> Conduit ProcessSource IO Bool
goodExitConduit ph = getProcessExitCode =$= awaitConduit =$= checkExitCode ph =$= fromMaybeBool

goodStderrConduit :: HasProcessHandler s a => s -> Conduit ProcessSource IO Bool
goodStderrConduit ph = getProcessStderr =$= awaitConduit =$= checkStderr ph =$= fromMaybeBool

-- | Given a `Source` and two `Conduit`s, zip the `Conduits` together, applied
-- to the `Source`, to make a new `Source`
zipToSource :: M.Monad m => Source m o -> Conduit o m a -> Conduit o m b -> Source m (a, b)
zipToSource = zipToSourceWith $ curry id

-- | Like `zipToSource`, but zipping together with a different function
-- than `(,)`
zipToSourceWith :: M.Monad m => (a -> b -> c) -> Source m o -> Conduit o m a -> Conduit o m b -> Source m c
zipToSourceWith f s c1 c2 = zipSources (s =$= c1) (s =$= c2) =$= map (uncurry f)


goodProcess :: HasProcessHandler p a => p  -> Source IO ProcessSource -> Source IO Bool
goodProcess ph s = zipToSourceWith (&&) s (goodExitConduit ph) (goodStderrConduit ph)


-- | `fuse`, inside of a `Conduit`
fusingConduit :: (M.Monad m, M.Monad n) => Conduit i m o -> Conduit (Source m i) n (Source m o)
fusingConduit c = map (=$= c)

-- | A `Conduit` with `ProcessSource` as input that applies `_procConduit`
processHandlerConduit :: HasProcessHandler p a => p -> Conduit ProcessSource IO (Source IO a)
processHandlerConduit ph = getProcessStdout =$= fusingConduit (ph ^. procConduit)

-- | Belongs in Data.Maybe.Utils
justIfTrue :: Bool -> a -> Maybe a
justIfTrue True x = Just x
justIfTrue _    _ = Nothing

-- | Use a `ProcessHandler` to convert a `Source` of `ProcessSource`s into
-- `Source` of `Maybe` result `Source`s.
processHandlerSource :: HasProcessHandler p a => p -> Source IO ProcessSource -> Source IO (Maybe (Source IO a))
processHandlerSource ph s = zipSources (goodProcess ph s) (s =$= processHandlerConduit ph) =$= map (uncurry justIfTrue)




-- (=$=) :: Monad m => Consumer a     m () -> Source   m    c    -> Conduit  a  m c
-- (=$=) :: Monad m => Source   m  b       -> Conduit  b  m c    -> Source   m  c
-- (=$=) :: Monad m => ConduitM a  b  m () -> ConduitM b  c m r  -> ConduitM a  c m r



