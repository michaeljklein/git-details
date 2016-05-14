{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Details where

import Control.Applicative (Alternative(..), many)
import Control.Monad
import Data.Attoparsec.Text ( digit
                            , IResult(..)
                            , Parser
                            , Result
                            , endOfInput
                            , isEndOfLine
                            -- , many'
                            , many1
                            , manyTill
                            , parse
                            , skip
                            , skipSpace
                            , string
                            , takeText
                            , takeTill
                            )
import Data.List.Split (Splitter, keepDelimsL, split, whenElt)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
-- import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Tree (Tree(..), Forest, unfoldForest)
import Prelude hiding (readFile)
import System.Directory
import System.Exit (ExitCode(..))
import System.Process
import TextShow
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Git.Commit

-- 1. make sure valid git dir (has .git dir)
-- 2. get project details: 'git remote show origin'
-- 3. get times/hashes for all commits: 'git log --pretty="%H<>%cd"' (hash<>date)
-- 4. ensure given file in dir
-- 5. attempt to parse current file (fail otherwise)
-- 6. for each commit, get and parse -> Data.Map: git show [hash]:[path] > new_path
-- 7. compile results into single tree
-- 8. output results

data Attribute = Attr { parent :: Maybe Attribute -- ^ The parent attribute. E.g. @test@ might be the parent of @dateTest@.
                      , name   :: T.Text          -- ^ The name of the attribute
                      , value  :: Maybe (forall a. TextShow a => a)
                      }


-- | Convert a list of attributes to a tree of attributes
treeify :: [Attribute] -> Tree Attribute
treeify = undefined

-- | Save a tree to a file (support filetypes?)
saveTree :: TextShow a => Tree a -> FilePath -> IO Bool
saveTree = undefined

-- | Check for @.git@ directory in current directory
isGitRootDir :: IO Bool
isGitRootDir = doesDirectoryExist ".git"

-- | Details of a @git@ project
data Details = Details { fetchURL         :: T.Text
                       , pushURL          :: T.Text
                       , headBranch       :: T.Text
                       , remoteBranch     :: T.Text
                       , localPullBranch  :: T.Text
                       , localPushBranch  :: T.Text
                       }

-- | Empty details (all fields are empty)
emptyDetails :: Details
emptyDetails = Details { fetchURL=""
                       , pushURL=""
                       , headBranch=""
                       , remoteBranch=""
                       , localPullBranch=""
                       , localPushBranch=""
                       }

-- | Skip any char
skipChar :: Parser ()
skipChar = skip (const True)

-- | Skip any @N@ chars
skipN :: Int -> Parser ()
skipN = flip replicateM_ skipChar

-- | Skip an entire line (up to and including the newline)
skipLine :: Parser ()
skipLine = do
  takeTill isEndOfLine
  skipChar

-- | Take an entire line (up to, but not including, the newline)
takeLine :: Parser T.Text
takeLine = do
  result <- takeTill isEndOfLine
  skipChar
  return result

-- | Parser for `Details`
-- | Examples parsed:
----
-- * remote origin
--   Fetch URL: https://github.com/michaeljklein/prim-spoon.git
--   Push  URL: https://github.com/michaeljklein/prim-spoon.git
--   HEAD branch: master
--   Remote branch:
--     master tracked
--   Local branch configured for 'git pull':
--     master merges with remote master
--   Local ref configured for 'git push':
--     master pushes to master (up to date)
----
----
-- * remote origin
--   Fetch URL: https://github.com/michaeljklein/CPlug.git
--   Push  URL: https://github.com/michaeljklein/CPlug.git
--   HEAD branch: master
--   Remote branch:
--     master tracked
--   Local branch configured for 'git pull':
--     master merges with remote master
--   Local ref configured for 'git push':
--     master pushes to master (fast-forwardable)
----
detailsParser :: Parser Details
detailsParser = do
  skipLine                            -- Skip "* remote origin"
  skipN 13                            -- Skip "  Fetch URL: "
  fetchURL'   <- takeLine
  skipN 13                            -- Skip "  Push  URL: "
  pushURL'    <- takeLine
  skipN 15                            -- Skip "  HEAD branch: "
  headBranch' <- takeLine
  skipLine                            -- Skip "  Remote branch:"
  skipSpace
  remoteBranch' <- takeLine
  skipLine                            -- Skip "  Local branch configured for 'git pull':"
  skipSpace
  localPullBranch' <- takeLine
  skipLine                            -- Skip "  Local branch configured for 'git pull':"
  skipSpace
  localPushBranch' <- takeLine
  skipChar `manyTill` endOfInput
  return $ Details { fetchURL=fetchURL'
              , pushURL=pushURL'
              , headBranch=headBranch'
              , remoteBranch=remoteBranch'
              , localPullBranch=localPullBranch'
              , localPushBranch=localPushBranch'
              }


-- | Do `readProcessWithExitCode`, returning @`Left` errorDetails@ on failure
simpleRun :: String -> [String] -> String -> IO (Either String String)
simpleRun cmd args input = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args input
  if (exitCode /= ExitSuccess) || (stdErr /= "")
  then do
    return . Left  $ unlines ["exitCode:", show exitCode, "stderr:", stdErr]
  else do
    return . Right $ stdOut

-- | Get the details of the @git@ project in the current directory
projectDetails :: IO (Result Details)
projectDetails = do
  maybeResults <- simpleRun "git" ["remote", "show", "origin"] ""
  case maybeResults of
    Left  _       -> return $ Done T.empty emptyDetails
    Right results -> return . parse detailsParser . T.pack $ results
-- projectDetails = do
--   (exitCode, out, err) <- readProcessWithExitCode "git" ["remote", "show", "origin"] ""
--   if (exitCode /= ExitSuccess) || (err /= [])
--   then do
--     return $ Done T.empty emptyDetails
--   else do
--     return . parseDetails . T.pack $ out

-- | Take until equal
takeTillEq :: Char -> Parser T.Text
takeTillEq = takeTill . (==)

-- | Returns @0@ on failed parse
unsafeDecimal :: Integral a => T.Text -> a
unsafeDecimal x = case decimal x of
                    Right (y, _) -> y
                    Left _       -> 0

-- | Check that the current version of the file exists and can be parsed
checkCurrentFile :: FilePath -> Parser a -> IO Bool
checkCurrentFile path parser = do
  exists <- doesFileExist path
  if exists
  then do
    contents <- readFile path
    case parser `parse` contents of
      Done _ _ -> return True
      _        -> return False
  else do
    return False

-- | Given a filepath (relative to the root directory of the branch) and a
-- `Commit`, return its contents or return `Nothing` on failure
getFileInCommit :: FilePath -> Commit -> IO (Either String T.Text)
getFileInCommit path (Commit {hash=hashText}) = do
  let hashStr = T.unpack hashText
  result <- simpleRun "git" ["show", hashStr ++ ":" ++ path] ""
  case result of
    Left  e -> return $ Left e
    Right s -> return . Right . T.pack $ s

-- | Like `getFileInCommit`, but also parses
parseFileInCommit :: FilePath -> Parser a -> Commit -> IO (Result a)
parseFileInCommit path parser commit = do
  text <- getFileInCommit path commit
  case text of
    Left  e -> return $ Fail T.empty [] e
    Right t -> return $ parser `parse` t

-- | Only insert the result into the `Map` if `Done`
insertResult :: Commit -> Result a -> Map.Map Commit a -> Map.Map Commit a
insertResult _ (Fail    _ _ _) m = m
insertResult _ (Partial _    ) m = m
insertResult c (Done    _ r  ) m = Map.insert c r m

-- | Given a `Parser`, a `FilePath`, and a `Commit` foldable (list), returns
-- a map from each `Commit` to the result of the given file being parsed by
-- the `Parser`.
parseForCommits :: Foldable t => Parser a -> FilePath -> t Commit -> IO (Map.Map Commit a)
parseForCommits parser path = foldM (\m c -> (parseCommit c >>= \r -> return $ insertResult c r m)) m0
  where
    parseCommit = parseFileInCommit path parser
    m0          = Map.empty






-- 7. compile results into single tree
-- 8. output results
