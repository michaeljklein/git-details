module Data.Plugin where



-- So a command line option has the following featurs:
-- command line interface (parser for options)
-- does work
-- outputs Attribute

-- Given command line options, here are the things that a plugin has access to
-- List of commits
-- Current attributes
-- Details
-- commit -> filename -> file
-- commit -> dirtree

mapToMap :: Ord a => (a -> b) -> [a] -> M.Map a b
mapToMap f = fromList . map (fmap f . join (,))

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
