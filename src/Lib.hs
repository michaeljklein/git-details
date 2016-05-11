{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative (many)
import Control.Monad
import Data.Attoparsec.Text (parse, skip, skipSpace, isEndOfLine, Parser(..), manyTill, endOfInput, Result(..), takeTill, IResult(..))
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
import Data.Time.Calendar (Day(..), fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Tree
import Prelude hiding (readFile)
import System.Directory
import System.Exit (ExitCode(..))
import System.Process
import TextShow
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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
    Left  error   -> return $ Done T.empty emptyDetails
    Right results -> return . parse detailsParser . T.pack $ results
-- projectDetails = do
--   (exitCode, out, err) <- readProcessWithExitCode "git" ["remote", "show", "origin"] ""
--   if (exitCode /= ExitSuccess) || (err /= [])
--   then do
--     return $ Done T.empty emptyDetails
--   else do
--     return . parseDetails . T.pack $ out

-- | `Commit` only contains the hash and date of the commit
data Commit = Commit { hash :: T.Text
                     , date :: UTCTime
                     } deriving (Eq, Show)

-- | As I'm not sure whether the hashes are ordered according to the dates,
-- this disambiguates the order. If there are any errors in this method,
-- they should be apparent when looking at the resulting graphs
instance Ord Commit where
  compare (Commit {hash=h1, date=d1}) (Commit {hash=h2, date=d2}) = case compare d1 d2 of
                                                                      LT -> LT
                                                                      GT -> GT
                                                                      EQ -> compare h1 h2

-- | Take until equal
takeTillEq :: Char -> Parser T.Text
takeTillEq = takeTill . (==)

-- | Returns @0@ on failed parse
unsafeDecimal :: Integral a => T.Text -> a
unsafeDecimal x = case decimal x of
                    Right (y, _) -> y
                    Left _       -> 0

-- | An example of what this should parse:
-- ~/../prim-spoon$ git log --date=iso --pretty="%H|%cd"
-- c6b082bcf72fed2db488dfd4506f9923f742e743|2016-05-03 19:25:24 -0400
-- 3f9778e19ee89000d8cd0a1c164afb3589650c3b|2016-05-02 18:42:11 -0400
-- abae2c404d4d2c26e7fc9005cfb59a5699977c39|2016-05-02 14:11:43 -0400
-- 07c0fe75d1a621abb10dfb4fb27549b390e35b3b|2016-05-02 13:48:43 -0400
-- c8bba65dc33f4c4cab53498002beddf673b674cc|2016-05-02 13:42:59 -0400
-- 3f207b6536fc30b2eadc09edd260e413c3e4ad79|2016-05-02 13:35:35 -0400
-- 04f4163c52231f1043123e6b9b66c76b95e3f05f|2016-05-02 13:32:37 -0400
-- f80e7e8c9204dd245a3545b5216e42bc0be7af2e|2016-05-02 13:28:25 -0400
-- e37b44908f08e912373c16a899516dc07fec363d|2016-05-02 13:04:31 -0400
-- 4604bec7ae5042aac493522cdf33166c26aa285f|2016-05-02 12:55:28 -0400
-- Consider taking all decimals instead of takeTillEq
parseDate :: Parser UTCTime
parseDate = do
  year  <- takeTillEq '-'
  skipChar
  month <- takeTillEq '-'
  skipChar
  day   <- takeTillEq ' '
  skipChar
  hour  <- takeTillEq ':'
  skipChar
  min   <- takeTillEq ':'
  skipChar
  sec   <- takeTillEq ' '
  skipLine
  let date  = fromGregorian (unsafeDecimal year) (unsafeDecimal month) (unsafeDecimal day)
  let time  = secondsToDiffTime $ (unsafeDecimal hour) * (60 * 60) + (unsafeDecimal min) * (60) + (unsafeDecimal sec)
  return $ UTCTime { utctDay = date, utctDayTime = time}


-- | Parse a single line returned from @git log@
parseLogLine :: Parser Commit
parseLogLine = do
  hash' <- takeTill (== '|')
  skipChar
  date' <- parseDate
  return $ Commit { hash=hash'
             , date=date'
             }

-- | This function gets all the `Commit`s for the current branch
getCommits :: IO (Result [Commit])
getCommits = do
  maybeResults <- simpleRun "git" ["log", "--date=short", "--pretty=\"%H|%cd\""] ""
  case maybeResults of
    Left  err     -> return $ Fail T.empty [] err
    Right results -> return . parse (many $ parseLogLine <* endOfInput) . T.pack $ results

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
