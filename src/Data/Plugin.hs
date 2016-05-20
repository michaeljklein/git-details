module Data.Plugin where


mapToMap :: Ord a => (a -> b) -> [a] -> M.Map a b
mapToMap f = fromList . map (fmap f . join (,))

-- Example plugins:
--  char counter
--  word counter
--  diff stats <<<<----- unsure whether to support this
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



-- |  Note: all the given @by_@ parameters must return `True` or be `Nothing`
-- for the info to be passed to the plugin.
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


data File = File FilePath Text deriving (Eq, Ord, Show)

-- | General type for a state in the pipes of the application.
data GitDetails ds pg cm dt fs pm pr rs = { _details :: ds
                                          , _plugins :: pg
                                          , _commit  :: cm
                                          , _diffTree :: dt
                                          , _file     :: fl
                                          , _plugResult :: pr
                                          }

makeLenses ''GitDetails

-- | Convenience names for making the dependencies
type Plugins'    = Array Int (Maybe PluginName)

type Commit'     = Maybe Commit

type DiffTrees'  = [DiffTree]

type File'       = Maybe File

type PlugResult' = Maybe Dynamic

-- | These are the convernience types, representing stages in the computation,
-- to ensure that the stages are executed in their proper order (i.e. as
-- `DiffTree`s requires the `Commit`s, `Commit`s should be computed first.
type DetailsS       = GitDetails X        X       X          X

type PluginS        = GitDetails Plugins' X       X          X

type CommitS        = GitDetails Plugins' Commit' X          X

type DiffTreesS     = GitDetails Plugins' Commit' DiffTrees' X

type FilePathS      = GitDetails Plugins' Commit' DiffTrees' X

type FileS          = GitDetails Plugins' Commit' DiffTrees' File'

type PluginMapperS  = GitDetails Plugins' Commit' DiffTrees' File' PlugResult'

type PluginReducerS = GitDetails Plugins' Commit' DiffTrees' File' PlugResult'

type ResultS        = GitDetails Plugins' Commit' DiffTrees' File' PlugResult'

type HPipe a = Pipe a a

type Pipe = HPipe GitDetails

-- git (info?)
detailsProducer :: Producer DetailsS

-- git log
commitProducer :: Pipe DetailsS CommitS

commitFilter :: Plugin -> Pipe CommitS CommitS

-- git diff-tree
diffTreeProducer :: Pipe CommitS DiffTreesS

diffTreeFilter :: Plugin -> Pipe DiffTreesS DiffTreesS

-- uses safe function (flag change?)
filePathPipe :: Pipe DiffTreeS FilePathS

filePathFilter :: Plugin -> Pipe FilePathS FilePathS

-- git checkout
filePipe :: Pipe FilePathS FileS

-- flag change
pluginsMapperPipe :: Pipe FileS PluginMapperS

pluginMapperPipe :: Plugin -> Pipe PluginMapperS PluginMapperS

-- flag change
pluginsReducerPipe :: Pipe PluginMapperS PluginReducerS

-- Better way to bundle (than cycling)
--                           /-> Pipe PluginReducerS PluginReducerS ->\
-- Producer PluginReducerS ----> Pipe PluginReducerS PluginReducerS ---> Pipe PluginReducerS PluginReducerS
--                           \-> Pipe PluginReducerS PluginReducerS ->/
-- That is, producer -fifo> worker reducers -fifo> single reducer
-- Functions like cat when plugin not in element
pluginReducerPipe :: Plugin -> Pipe PluginReducerS PluginReducerS

-- flag change
resultsPipe :: Pipe PluginS ResultS

-- folder (probably concat/next/sort)
resultsReducer :: Producer' ResultS m r -> Results











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



