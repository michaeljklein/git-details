module Data.Plugin where


mapToMap :: Ord a => (a -> b) -> [a] -> M.Map a b
mapToMap f = fromList . map (fmap f . join (,))

-- Example plugins:
--  char counter
--  word counter
--  diff stats <<<<-----
--  cloc
--  tree
--  dependency checker -- out of scope? YES
--
--  Conclusion: Do NOT allow plugins multiple requests or anything other than a stream filter


{-# DEPRECATED Mode "Need to update this type for cmdargs" #-}
data Mode a

type PluginPipe s t = Pipe (LocalInput s) (LocalState t)

-- | The `Plugin` data type contains everything that a plugin needs to be
-- integrated into the tool (including lenses)
data Plugin s a = Plug { _plugName    :: Text                                      -- Plugin name
                       , _parser      :: Mode a                                    -- Command-line mode
                       , _wanted      :: Details -> a -> Wanted                    -- What files/details the plugin wants
                       , _plugMapper  :: Details -> a -> PluginPipe s s            -- The main pipe of the plugin, consuming local state/input and producing localstate
                       , _plugReducer :: Details -> a -> Maybe (PluginPipe [s] s)  -- If `Just`, plugin will be run in mapReduce fashion
                       }

makeClassy ''Plugin




instance Ord (Plugin s a) where
  compare x y = compare (x^.plugName) (y^.plugName)


-- | This is what a `Plugin` pipe can access. It's updated and fed back into
-- the pipe at each step
data LocalInput a = LocInput { _commit         :: Maybe Commit
                             , _commitDiffTree :: Maybe DiffTree
                             , _commitDirTree  :: Maybe DirTree
                             , _commitFiles    :: Maybe Map Path Text
                             , _localState     :: LocalState a
                             }

makeClassy ''LocalInput


-- | This is the local state for a `Plugin`
data LocalState a = LocalState { _state :: a
                               , _attributes :: [Attribute]
                               }

makeClassy ''LocalState


-- | This allows a plugin to finely specify what it wants from the project
-- and how it's choosing. Because of how fine this is, fetchers may quickly
-- filter the files/commits for piping to the plugins. Note: all the given
-- @by_@ parameters must return `True` for the info to be passed to the plugin.
-- If diffTree, dirTree, files are all false, only (Details, Commit) will be
-- passed to the plugin.
data Wanted = Want { _diffTree        :: Bool                    -- ^ The `DiffTree` results
                   , _dirTree         :: Bool                    -- ^ The `DirectoryTree`
                   , _files           :: Bool                    -- ^ File results (a plugin could only access diffs or the trees)
                   , _wantBy          :: WantedBy
                   }

data WantedBy = WantBy { _byCommit        :: Maybe (Commit -> Bool)  -- ^ Filter by `Commit`
                       , _byFileExtension :: Maybe (Text -> Bool)    -- ^ Filter by file extension
                       , _byFileName      :: Maybe (Text -> Bool)    -- ^ Filter by full file name
                       , _byDirectoryPath :: Maybe (Path -> Bool)    -- ^ Filter by directory path
                       , _byDiffTree      :: Maybe (DiffTree -> Bool)-- ^ Filter by the `DiffTree`
                       }

makeLenses ''Wanted


-- master:
--  plugin list is input
--  sorts plugins into simple/mapreduce
--  makes pipes
--  forks pipes
--  connects pipes


-- pipes
--  (commit, [plugin]) producer
--  byCommit filter
--  (commit, difftree, [plugin]) <- which want difftrees
--
--  Don't forget an 'anyWants'
--  byFileExtension >-> byFileName = byFile
--
-- details producer -> commit pipe -> \                                  / onlyCommit (commit, [plugin]) -> (plugin, commit) -> plugin cycle0
--                         initialSort > (commit, [plugin]) -> byCommit <
-- plugin  producer ----------------> /                                  \ diffTreePipe
--
--                                                          / onlyDiffTree (commit, difftree, [plugin]) -> (plugin, (commit, difftree)) -> plugin cycle1
-- diffTreePipe (commit, difftree, [plugin]) -> byDiffTree <
--                                                          \ dirTreePipe
--
--                                                         / onlyDirTree (commit, [difftree], dirtree, [plugin]) -> (plugin, (commit, [difftree], dirtree)) -> plugin cycle2
-- dirTreePipe (commit, [difftree], dirtree, [plugin]) -> byDirTree <
--                                                         \ filePipe
--
--                                                                   / onlyFiles (commit, [difftree], dirtree, file, [plugin]) -> (plugin, (commit, [difftree], ..)) -> plugin cycle3
-- filePipe (commit, [difftree], dirtree, file, [plugin]) -> byFile <
--                                                                   \ nullPipe
--
--
--                                 / onlyStuffPipe
-- forkedPipe x (x, _) -> byStuff <
--                                 \ nextPipe
--
-- forkedPipe needs:
--  up fork pipe
--  down fork pipe
--
--  forkedPipe :: Pipe a b -> Pipe a c -> Consumer' a m ()
--
-- onlyStuffPipe (commit, .., [plugin]) -> (plugin, (commit, ..)) -> plugin cycle
--
--
-- plugin cycle -> plugin1 -> plugin2 -> plugin3
--                       \          |          /
--                        \         |         /
--                         \/       V       \/
--                           results mailbox

splitterPipe :: Pipe a (a, a)
splitterPipe = Pipes.Prelude.Map $ join (,)

producerToTQueue :: Producer b m r -> STM (TQueue b)

whileM_ :: Monad m => (a -> m Bool) -> m a -> m a
whileM_ p x = do
  y <- x
  t <- p y
  if t
  then do
    whileM_ p x
  else do
    return y

duplicateInput :: Input a -> Output a -> Output a -> STM ()
duplicateInput input out1 out2 = whileM_ return duplicateOneOutput
  where
    duplicateOneOutput = do
      maybeVal <- recv input
      case maybeVal of
        Just x  -> do
          sent1 <- send out1 x
          sent2 <- send out2 x
          return $ sent1 && sent2
        Nothing -> return False


forkProducer :: (Consumer' b m r, Producer' b m r, Producer' b m r)
forkProducer p = (consumer, producer1, producer2)
  where

(output0 , input0) <- spawn unlimited
(output1, input1) <- spawn unlimited
(output2, input2) <- spawn unlimited

consumer = toOutput output0

duplicateInput input0 output1 output2

producer1 = toInput input1
producer2 = toInput input2


mergeProducers = Pipes.Prelude.zipWith

-- idea design for splitter: (from left/right)
--
--
--                                                                      / Producer b1
-- Producer' (Either b1 b2) m r -> Pipe (Either b1 b2) (Either b1 b2) ->
--                              /                                       \
--                              \                                        |
--                               <-Pipe (Either b1 b2) (Either b1 b2) <-/
--                              /
--                   Producer b2

forkPipe :: Pipe a (a, a)
forkPipe = Pipes.Prelude.map $ join (,)


forkPipes :: Pipe a b -> Pipe c d -> Pipe (a, c) (b, d)
forkPipes


detailsProducer :: Producer Details
commitPipe :: Pipe Details (Details, Commit)
pluginProducer :: Producer Plugin
initialSort :: Pipe () (Details, Commit, [Plugin])
byCommit :: Pipe (Details, Commit, [Plugin]) (Details, Commit, [Plugin])



predToMaybe :: (a -> Bool) -> b -> a -> Maybe b
predToMaybe p x y | p y       = Just x
                  | otherwise = Nothing

predToList :: (a -> Bool) -> b -> a -> [b]
predToList p x y | p y       = [x]
                 | otherwise = []


initialSorter :: (a -> b -> Bool) -> [b] -> a -> (a, [b])
initialSorter p ys x = (x, filter (p x) ys)

pipeInitialSorter :: (a -> b -> Bool) -> [b] -> Pipe a (a, [b])
pipeInitialSorter p ys = Pipes.Prelude.map $ initialSorter p ys


sorter :: (a -> b -> Bool) -> (a, [b]) -> (a, [b])
sorter p ~(a, bs) = (a, filter (p a) bs)

pipeSorter :: (a -> b -> Bool) -> Pipe (a, [b]) (a, [b])
pipeSorter p = Pipes.Prelude.map $ sorter p

-- | @maybePred pred x@ applies @pred@ if it's `Just` a predicate
-- otherwise, it returns `True`
maybePred :: Maybe (a -> Bool) -> a -> Bool
maybePred (Just p) x = p x
maybePred  _       _ = True

commitProducer :: Producer Commit


commitSorter :: [Plugin] -> Pipe Commit (Commit, [Plugin])
commitSorter ps = pipeInitialSorter pred ps
  where
    pred c p = maybePred (p^.byCommit) c

diffTreePipe :: Pipe Commit (Commit, DiffTree)
diffTreePipe = Pipes.Prelude.mapM commitToDiffTrees


-- HOW to balance load?
-- Well, simply make a mailbox of commits
-- assume that most of the load is in fetching
-- consider multi-stage plugins with a fast merger (like counters, or sorting to merge)
-- definately have a map/reduce plugin sorta thing
-- This should be relatively quick once the basics are pumped out


-- Filter order
--  commit date
--  commit hash
--  commit
--  difftree
--  fileName
--  fileExtension
--  dirpath

-- The idea with these filters is that once a file is rejected,
-- it's always rejected. This means things like diffs allow us
-- to skip most of the info before it gets to more intensive
-- processing.


-- | Very Zen, but in this case, very possibly pointless.
--  (Used to build what's `Wanted` for a plugin.)
wantNothing :: Wanted
wantNothing = Want False False False False False n n n n n n n
  where
    n = Nothing


