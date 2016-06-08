module Data.Plugin where

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

-- | The `Plugin` data type contains everything that a plugin needs to be
-- integrated into the tool (including lenses)
data Plugin s a = Plug { _plugName    :: Text                                      -- Plugin name
                       , _parser      :: Mode a                                                 -- Command-line mode
                       , _wanted      :: Details -> a -> WantedBy                               -- What files/details the plugin wants
                       , _plugMapper  :: Details -> a -> Conduit PluginMapperS IO PluginMapperS -- The main pipe of the plugin, consuming local state/input and producing localstate
                       , _plugReducer :: Details -> a -> Maybe (PluginPipe [s] s)  -- If `Just`, plugin will be run in mapReduce fashion
                       }

makeClassy ''Plugin

{-# LANGUAGE ExistentialQuantification #-}
class (ToJSON a, TextShow a) => IsPluginResult a

data PluginResult = forall a. IsPluginResult a => PluginResult { _unpackresult :: a }

packResult :: (ToJSON a, TextShow a) => a -> PluginResult
packResult = PluginResult

data Plugin c i o = Plug { _plugName  :: Text
                         , _cmdParser :: Mode c
                         , _wants     :: Details -> c -> Wanted
                         , _mapper    :: Details -> c -> FileS -> IO i
                         , _seed      :: IsPluginResult o => Details -> c -> o
                         , _reducer   :: IsPloginResult o => Details -> c -> o -> i -> IO o
                         }


instance Ord (Plugin c i o) where
  compare x y = compare (x^.plugName) (y^.plugName)



-- -- |  Note: all the given @by_@ parameters must return `True` or be `Nothing`
-- -- for the info to be passed to the plugin.
-- data Wanted = Want { _diffTree        :: Bool                    -- ^ The `DiffTree` results
--                    , _dirTree         :: Bool                    -- ^ The `DirectoryTree`
--                    , _files           :: Bool                    -- ^ File results (a plugin could only access diffs or the trees)
--                    , _wantBy          :: WantedBy
--                    }

data Wanted = Want { _byCommit        :: Maybe (Commit -> Bool)  -- ^ Filter by `Commit`
                   , _byFileExtension :: Maybe (Text -> Bool)    -- ^ Filter by file extension
                   , _byFileName      :: Maybe (Text -> Bool)    -- ^ Filter by full file name
                   , _byDirectoryPath :: Maybe (Path -> Bool)    -- ^ Filter by directory path
                   , _byDiffTree      :: Maybe (DiffTree -> Bool)-- ^ Filter by the `DiffTree`
                   }

makeLenses ''Wanted


-- | Very Zen, but in this case, very possibly pointless.
--  (Used to build what's `Wanted` for a plugin.)
wantNothing :: Wanted
wantNothing = Want n n n n n
  where
    n = Nothing

data File = File FilePath Text deriving (Eq, Ord, Show)

-- | General type for a state in the pipes of the application.
data GitDetails ds pg cm dt fl pm pr rs = { _details    :: ds
                                          , _plugins    :: pg
                                          , _commit     :: cm
                                          , _diffTree   :: dt
                                          , _file       :: fl
                                          , _plugMapped :: pm
                                          , _plugResult :: pr
                                          , _allResults :: rs
                                          }

makeLenses ''GitDetails

GitDetails Details Plugins' Commit' DiffTrees' File' PlugMapped' PlugResult' AllResults'

-- | Convenience names for making the dependencies
type Plugins'    = Array Int (Maybe PluginName)

type Commit'     = Maybe Commit

type DiffTrees'  = [DiffTree]

type File'       = Maybe File

type PlugMapped' = Maybe Dynamic

type PlugResult' = Maybe PluginResult

type AllResults' = Maybe Value

-- | These are the convernience types, representing stages in the computation,
-- to ensure that the stages are executed in their proper order (i.e. as
-- `DiffTree`s requires the `Commit`s, `Commit`s should be computed first.
--
type DetailsS      = GitDetails Details X        X       X          X     X           X           X

type PluginS       = GitDetails Details Plugins' X       X          X     X           X           X

type CommitS       = GitDetails Details Plugins' Commit' X          X     X           X           X

type DiffTreeS     = GitDetails Details Plugins' Commit' DiffTrees' X     X           X           X

type FileS         = GitDetails Details Plugins' Commit' DiffTrees' File' X           X           X

type PluginMapS    = GitDetails Details Plugins' Commit' DiffTrees' File' PlugMapped' X           X

type PluginResultS = GitDetails Details Plugins' Commit' DiffTrees' File' PlugMapped' PlugResult' X

type ResultS       = GitDetails Details Plugins' Commit' DiffTrees' File' PlugMapped' PlugResult' AllResults'

-- | repeated source of Details
detailsS :: Source IO DetailsS

-- | adds plugin array to each DetailS
pluginC :: Conduit DetailsS IO PluginS

-- | adds one commit to each PluginS, closes backward when done
commitC :: Conduit PluginS IO CommitS

-- | for each plugin, one filter to check whether the plugin wants it
filterCommitC :: Plugin -> Conduit CommitS IO CommitS

-- | gets [DiffTree] of each CommitS
diffTreeC :: Conduit CommitS IO DiffTreeS

-- | for each plugin, one filter to check whether the plugin wants it
filterDiffTreeC :: Plugin -> Conduit DiffTreeS IO DiffTreeS


-- | unfolder Conduit? i.e. Conduit [a] m a

headCE :: (Monad m, IsSequence seq) => Consumer seq m (Maybe (Element seq))


(=$=) :: Monad m => ConduitM a  b m () -> ConduitM b c m () -> ConduitM a  c m ()
(=$=) :: Monad m => ConduitM () b m () -> ConduitM b c m () -> ConduitM () c m ()

type Source m o = ConduitM () o m ()
type Conduit i m o = ConduitM i o m ()
type Sink i m r = ConduitM i Void m r

source, sink

Conduit seq IO (Source IO (Element seq))

yieldMany :: (Monad m, MonoFoldable mono) => mono -> Producer m (Element mono)

map yieldMany :: (Monad m1, Monad m2, MonoFoldable mono) => Conduit mono m1 (Producer m (Element mono))

elementS :: MonoFoldable mono => Source IO mono -> IO (Source IO (Element mono))
elementS s = do
  queue <- newTQueueIO
  let sink = sinkTQueue queue :: Sink a IO ()
  mapC yieldMany s $$ mapM_C ($$ sink) :: IO ()
  return $ sourceTMQueue queue




-- git (info?)
detailsProducer :: MonadIO m => Producer m DetailsS
detailsProducer

gitDetailsProc :: CreateProcess
gitDetailsProc = proc "git" ["remote", "show", "origin"]

hclose stdin'
hclose stderr'


-- | read/write
createPipe :: IO (Handle, Handle)

-- | Creates a pipe, then closes both ends. Returned in format: (output, input)
createEmptyHandles :: IO (Handle, Handle)
createEmptyHandles = do
  (o, i) <- createPipe
  hClose o
  hClose i
  return (o, i)

repeatC (d :: Details)

gitDetailsProcess :: IO ProcessHandle
gitDetailsProcess = spawnProcess "git" ["remote", "show", "origin"]

-- | Get the details of the @git@ project in the current directory
-- projectDetails :: IO (Either String Details)
-- projectDetails = do
--   maybeResults <- simpleRun "git" ["remote", "show", "origin"] ""
--   case maybeResults of
--     Left  err     -> return $ Left err
--     Right results -> return . parseOnly detailsParser . T.pack $ results


-- git log
commitProducer :: Monad m => Conduit DetailsS m CommitS

commitFilter :: Monad m => Plugin -> Conduit CommitS m CommitS

-- git diff-tree
diffTreeProducer :: Monad m => Conduit CommitS m DiffTreesS

diffTreeFilter :: Monad m => Plugin -> Conduit DiffTreesS m DiffTreesS

-- uses safe function (flag change?)
filePathPipe :: Monad m => Conduit DiffTreeS m FilePathS

filePathFilter :: Monad m => Plugin -> Conduit FilePathS m FilePathS

-- git checkout
filePipe :: Monad m => Conduit FilePathS m FileS

-- flag change
pluginsMapperPipe :: Monad m => Conduit FileS m PluginMapperS

pluginMapperPipe :: Monad m => Plugin -> Conduit PluginMapperS m PluginMapperS

-- flag change
pluginsReducerPipe :: Monad m => Conduit PluginMapperS m PluginReducerS

-- Better way to bundle (than cycling)
--                             /-> Conduit PluginReducerS m PluginReducerS ->\
-- Producer m PluginReducerS ----> Conduit PluginReducerS m PluginReducerS ---> Conduit PluginReducerS m PluginReducerS
--                             \-> Conduit PluginReducerS m PluginReducerS ->/
-- That is, producer -fifo> worker reducers -fifo> single reducer
-- Functions like cat when plugin not in element
pluginReducerPipe :: Monad m => Plugin -> Conduit PluginReducerS m PluginReducerS

-- flag change
resultsPipe :: Monad m => Conduit PluginS m ResultS

-- folder (probably concat/next/sort)
resultsReducer :: Monad m => Consumer m ResultS









predToMaybe :: (a -> Bool) -> b -> a -> Maybe b
predToMaybe p x y | p y       = Just x
                  | otherwise = Nothing

predToList :: (a -> Bool) -> b -> a -> [b]
predToList p x y | p y       = [x]
                 | otherwise = []


initialSorter :: (a -> b -> Bool) -> [b] -> a -> (a, [b])
initialSorter p ys x = (x, filter (p x) ys)

sorter :: (a -> b -> Bool) -> (a, [b]) -> (a, [b])
sorter p ~(a, bs) = (a, filter (p a) bs)

-- | @maybePred pred x@ applies @pred@ if it's `Just` a predicate
-- otherwise, it returns `True`
maybePred :: Maybe (a -> Bool) -> a -> Bool
maybePred (Just p) x = p x
maybePred  _       _ = True


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





