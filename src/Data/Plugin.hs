module Data.Plugin where



-- So a command line option has the following featurs:
-- command line interface (parser for options)
-- does work
-- outputs Attribute


mapToMap :: Ord a => (a -> b) -> [a] -> M.Map a b
mapToMap f = fromList . map (fmap f . join (,))


-- main question: how to organize the fetchers?
-- Don't want to fetch multiple times
-- Don't want to wait for plugins
--
-- question then is: how much power to give to the plugins?
--  If plugins have `fileWanted :: InitialDetails -> Commit -> Path -> Bool`, will be extremely fast


-- Example plugins:
--  char counter
--  word counter
--  diff stats <<<<-----
--  cloc
--  tree
--  dependency checker -- out of scope? YES
--
--  Conclusion: Do NOT allow plugins multiple requests or anything other than a stream filter


-- Plugin:
--  parse :: Parser a
--  filesWanted :: FilesWanted
--  pipe :: a -> (FilesGiven + State) -> Producer [Attribute]
--

{-# DEPRECATED Mode "Need to update this type for cmdargs" #-}
data Mode a

data Plugin s a = Plug { plugName :: Text                                                      -- Plugin name
                       , parser   :: Mode a                                                    -- Command-line mode
                       , wanted   :: Details -> a -> Wanted                                    -- What files/details the plugin wants
                       , plugPipe :: Details -> Pipe (LocalInput, LocalState a) (LocalState a) -- The main pipe of the plugin, consuming local state/input and producing localstate
                       }

data LocalInput = LocalInput { commit   :: Maybe Commit
                             , diffTree :: Maybe DiffTree
                             , dirTree  :: Maybe DirTree
                             , files    :: Maybe Map Path Text
                             }

data LocalState a = LocalState { state :: a
                               , attributes :: [Attribute]
                               }

data Wanted = Want { diffTree        :: Bool                    -- ^ The `DiffTree` results
                   , dirTree         :: Bool                    -- ^ The `DirectoryTree`
                   , files           :: Bool                    -- ^ File results (a plugin could only access diffs or the trees)
                   , byCommit        :: Maybe (Commit -> Bool)  -- ^ Filter by `Commit`
                   , byCommitHash    :: Maybe (SHA1 -> Bool)    -- ^ Filter by `SHA1` hash
                   , byCommitDate    :: Maybe (UTCTime -> Bool) -- ^ Filter by `UTCTime` date
                   , byFileExtension :: Maybe (Text -> Bool)    -- ^ Filter by file extension
                   , byFileName      :: Maybe (Text -> Bool)    -- ^ Filter by full file name
                   , byDirectoryPath :: Maybe (Path -> Bool)    -- ^ Filter by directory path
                   , byDiffTree      :: Maybe (DiffTree -> Bool)-- ^ Filter by the `DiffTree`
                   }

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

-- by commit hash/data
-- by filename
-- diffs



-- master
--  runs event loop (get, push to plugins, feedback, next commit)
--  manages fetching of files
--  updates global read variables

-- plugin
--  gets opened with results of parser, details
--  gets current commit, local attributes, dirtree, etc.
--  can request files (or marks files as wanted/unwanted?
--  somehow updates stuff, requests, and continues.
--  basically, has full access to local, read only to global, some local exported to update local

-- fetchers
--  gets stream of requests (in order, somehow)
--  streams out in hierarchy

-- So we have Map Plugin FileName.
-- Want to convert to Map FileName Plugin
--
-- Or, we have lots of (Plugin, Array (FileName, Bool))
-- Then merged, pretty easily to Array FileName [Plugin]
-- Then unmerged by streaming to each plugin
-- Also streamed much more easily
-- Yeah, I like these streams, very fast
-- Require that they're ordered, so that there's little blocking
--

-- filenames are max 255 bytes, make fixed length? hmmmm.... CStrLn is not too bad.....

-- (plugin) request :: LocalState -> Stream (FileName, Maybe PluginName)
-- merge :: Stream (FileName, Maybe PluginName) -> Stream (FileName, Maybe PluginName) -> Stream (FileName, [PluginName])
-- unmerge -- stream back to plugins

-- This is read-only on local level, updated only by master
data CLILocalState  { details    :: Details
                    , commit     :: Commit
                    , dirtree    :: DirectoryTree
                    , files      :: M.Map FilePath Text
                    }

-- These are read/write, though local to each plugin
-- These may be read
                    , requests   :: [FilePath]
                    , attributes :: [Attribute]

-- This may only be accessed by master
data CLIGlobalState { details    :: Details
                    , commits    :: Commit
                    , attributes :: Map PluginName [Attribute]
                    , dirTree    :: DirectoryTree
                    , requests   :: Map PluginName [FilePath]
                    , files      :: M.Map Path Text
                    }


commitInfo :: Commit -> (DirectoryTree, Map FilePath Text)
commitInfo

mapToMap commitInfo

data CLIState { details    :: Details
              , commit     :: Commit
              , commits    :: [Commit]
              , attributes :: [Attribute]
              , dirTree    :: DirectoryTree
              , files      :: M.Map FilePath Text
              }

-- Parse commands
-- For each active plugin,
--  for each commit.
--    pass commits to plugins
--    lazy contents to plugins
--    update CLIState


data CLIState = CLIState { details    :: Details
                         , commits    :: [Commit]
                         , attributes :: [Attribute]
                         , getFile    :: M.Map (Commit, FilePath) Text
                         , getDirTree :: M.Map  Commit            DirectoryTree
                         }

Map Commit (DirectoryTree, Map FilePath Text)



data Plugin a = { parser :: Parser a
                ,
