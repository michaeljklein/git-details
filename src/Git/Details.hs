{-|
Module      : Git.Details
Description : Functions and data types for getting the details of git projects
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Details where

import Control.Applicative (many)
import Control.Monad (foldM)
import Data.Attoparsec.Text ( Parser
                            , endOfInput
                            , parseOnly
                            )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Git.Types (Commit(..), SHA1(..))
import Git.Types.Parse (parseLogLine)
import Prelude hiding (readFile)
import System.Directory (doesDirectoryExist, doesFileExist)
-- import System.Process.Utils (simpleRun)
import Data.Git.Details (Details(..))
import Data.Git.Details.Parse (detailsParser)

import Control.Lens.Operators ((.~), (&))
import Data.Conduit (Conduit, Producer, Source, (=$=), yield)
import Data.Conduit.Process.Utils
import Data.ByteString (ByteString)
import Conduit (encodeUtf8C, lift)
import Control.Exception.Base (SomeException)
import Data.Conduit.Attoparsec (ParseError, conduitParserEither)

-- 1. make sure valid git dir (has .git dir)
-- 2. get project details: 'git remote show origin'
-- 3. get times/hashes for all commits: 'git log --pretty="%H<>%cd"' (hash<>date)
-- 4. ensure given file in dir
-- 5. attempt to parse current file (fail otherwise)
-- 6. for each commit, get and parse -> Data.Map: git show [hash]:[path] > new_path
-- 7. compile results into single tree
-- 8. output results
-- -- | `Attribute`s can be unfolded into a `Tree` and otherwise are named values.
-- data Attribute = Attr { parent :: Maybe Attribute -- ^ The parent attribute. E.g. @test@ might be the parent of @dateTest@.
--                       , name   :: T.Text          -- ^ The name of the attribute
--                       , value  :: Maybe (forall a. TextShow a => a) -- ^ The value of the input, which must be a member of the class `TextShow`
--                       }

-- | Check for @.git@ directory in current directory
isGitRootDir :: IO Bool
isGitRootDir = doesDirectoryExist ".git"

-- isGitRootDir2  --Producer IO Bool
isGitRootDir2 = undefined --lift $ doesDirectoryExist ".git"


-- | Get the details of the @git@ project in the current directory
-- projectDetails :: IO (Either String Details)
-- projectDetails = do
--   maybeResults <- simpleRun "git" ["remote", "show", "origin"] ""
--   case maybeResults of
--     Left  err     -> return $ Left err
--     Right results -> return . parseOnly detailsParser . T.pack $ results


-- singleCmdParse :: String -> Parser a -> Source IO (Either SomeException a)
-- singleCmdParse c p = yield c =$= shellC =$= processSourceC & processHandlerS ph
--   where
--     ph = defaultParserPH p

-- -- | A `ProcessHandler` built from `defaultCmd` and the provider `Text` parser
-- defaultParserPH :: Parser a -> ProcessHandler (Either ParseError a)
-- defaultParserPH p = defaultCmd & procC .~ (encodeUtf8C =$= conduitParserEither p)

-- projectDetailsS :: Source IO (Either SomeException Details)
-- projectDetailsS = singleCmdParse "git remote show origin" detailsParser




{-
-- | Given a filepath (relative to the root directory of the branch) and a
-- `Commit`, return its contents or return `Nothing` on failure
getFileInCommit :: FilePath -> Commit -> IO (Either String T.Text)
getFileInCommit path (Commit {hash=(SHA1 hashText)}) = do
  let hashStr = T.unpack hashText
  result <- simpleRun "git" ["show", hashStr ++ ":" ++ path] ""
  case result of
    Left  e -> return $ Left e
    Right s -> return . Right . T.pack $ s

-- | Like `getFileInCommit`, but also parses
parseFileInCommit :: FilePath -> Parser a -> Commit -> IO (Either String a)
parseFileInCommit path parser commit = do
  text <- getFileInCommit path commit
  case text of
    Left  e -> return $ Left e
    Right t -> return $ parser `parseOnly` t

-- | Only insert the result into the `Map` if `Done`
insertResult :: Commit -> Either String a -> Map.Map Commit a -> Map.Map Commit a
insertResult c (Right r) m = Map.insert c r m
insertResult _  _        m = m

-- | Given a `Parser`, a `FilePath`, and a `Commit` foldable (list), returns
-- a map from each `Commit` to the result of the given file being parsed by
-- the `Parser`.
parseForCommits :: Foldable t => Parser a -> FilePath -> t Commit -> IO (Map.Map Commit a)
parseForCommits parser path = foldM (\m c -> (parseCommit c >>= \r -> return $ insertResult c r m)) m0
  where
    parseCommit = parseFileInCommit path parser
    m0          = Map.empty

-- | This function gets all the `Commit`s for the current branch
-- An example of what this should parse:
-- @
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
-- @
--
getCommits :: IO (Either String [Commit])
getCommits = do
  maybeResults <- simpleRun "git" ["log", "--date=short", "--pretty=\"%H|%cd\""] ""
  case maybeResults of
    Left  err     -> return $ Left err
    Right results -> return . parseOnly (many $ parseLogLine <* endOfInput) . T.pack $ results


-- | Check that the current version of the file exists and can be parsed
checkCurrentFile :: FilePath -> Parser a -> IO Bool
checkCurrentFile path parser = do
  exists <- doesFileExist path
  if exists
  then do
    contents <- readFile path
    case parser `parseOnly` contents of
      Right _  -> return True
      _        -> return False
  else do
    return False
-}
