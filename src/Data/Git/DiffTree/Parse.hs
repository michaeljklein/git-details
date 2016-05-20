{-|
Module      : Git.DiffTree.Parse
Description : Parsers for the results of @git diff-tree@
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}


module Data.Git.DiffTree.Parse where


import Control.Applicative        ( (<|>)
                                  )
import Control.Monad              ( liftM
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , char
                                  , count
                                  , digit
                                  , many'
                                  , skipSpace
                                  )
import Data.Attoparsec.Text.Utils ( failParse
                                  , maybeParse
                                  , takeLine
                                  )
import Git.Types.Parse            ( parseDiffMode
                                  , parsePath
                                  , parseSHA1
                                  )
import Data.Git.DiffTree          ( Percent(..)
                                  , Status(..)
                                  , DiffCommit(..)
                                  , DiffTree(..)
                                  )
import Git.Types                  ( SHA1(..)
                                  )


-- | Parse a percentage
parsePercent :: Parser Percent
parsePercent = do
  p <- count 2 digit
  return . Percent . read $ p


-- | Parse a single status, without percentage
parseOneStatus :: Char -> Status -> Parser Status
parseOneStatus c s = char c >> return s

-- | Parse the `Modify` status
parseModifyStatus :: Char -> (Maybe Percent -> Status) -> Parser Status
parseModifyStatus 'M' _ = char 'M' >> liftM (Modify . Just) parsePercent <|> return (Modify Nothing)
parseModifyStatus  _  _ = failParse -- 'M' must be passed to parseModifyStatus

-- | Parse the `Copy` and `Rename` statuses
parseOnePercentStatus :: Char -> (Percent -> Status) -> Parser Status
parseOnePercentStatus 'C' _ = char 'C' >> liftM Copy   parsePercent
parseOnePercentStatus 'R' _ = char 'R' >> liftM Rename parsePercent
parseOnePercentStatus  _  _ = failParse -- Char other than 'C'/'R' passed to parseOnePercentStatus

-- | All possible status parsers
statusParsers :: [Parser Status]                       -- Possible status letters are:
statusParsers = [ parseOneStatus        'A' Addition   -- A: addition of a file
                , parseOnePercentStatus 'C' Copy       -- C: copy of a file into a new one
                , parseOneStatus        'D' Delete     -- D: deletion of a file
                , parseModifyStatus     'M' Modify     -- M: modification of the contents or mode of a file
                , parseOnePercentStatus 'R' Rename     -- R: renaming of a file
                , parseOneStatus        'T' TypeChange -- T: change in the type of the file
                , parseOneStatus        'U' Unmerged   -- U: file is unmerged (you must complete the merge before it can be committed)
                , parseOneStatus        'X' Unknown    -- X: "unknown" change type (most probably a bug, please report it)
                ]

-- | Parse any `Status`
parseStatus :: Parser Status
parseStatus = foldl1 (<|>) statusParsers


-- | Parse a `DiffTree` See specification at:
-- @https://git-scm.com/docs/diff-format/2.5.3@
parseDiffTree :: Parser DiffTree
parseDiffTree = do
  _ <- char ':'
  srcMode' <- parseDiffMode
  _ <- char ' '
  dstMode' <- parseDiffMode
  _ <- char ' '
  srcSHA1' <- parseSHA1
  _ <- char ' '
  dstSHA1' <- parseSHA1
  _ <- char ' '
  status'  <- parseStatus
  skipSpace
  srcPath' <- parsePath
  dstPath' <- maybeParse parsePath
  let src' = DiffCommit srcMode' srcSHA1' $ Just srcPath'
  let dst' = DiffCommit dstMode' dstSHA1'        dstPath'
  return $ DiffTree src' dst' status'

-- | Parse a collection of `DiffTree` results
parseDiffTrees :: Parser (SHA1, [DiffTree])
parseDiffTrees = do
  sha1 <- takeLine
  diffTrees <- many' parseDiffTree
  return (SHA1 sha1, diffTrees)

