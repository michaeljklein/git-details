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


-- | Very Zen, but in this case, very possibly pointless.
--  (Used to build what's `Wanted` for a plugin.)
wantNothing :: Wanted
wantNothing = Want False False False False False n n n n n n n
  where
    n = Nothing

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


-- git (info?)
detailsProducer :: Monad m => Producer m DetailsS

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





