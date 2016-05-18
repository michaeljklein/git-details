{-|
Module      : Git.DiffTree
Description : Types and parsers for the results of @git diff-tree@
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Git.DiffTree where

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

import Git.Types (Mode, Path, SHA1(..))
import Git.Types.Parse (parseMode, parsePath, parseSHA1)

------------------------------------------------------------------------------------------------------------------------------------------------------


-- | The source or destination of an update/change, as shown by @git tree-diff@
data DiffCommit = DiffCommit { diffMode :: Mode
                             , diffSHA1 :: SHA1
                             , diffPath :: Maybe Path
                             } deriving (Eq, Ord, Show)


-- -- | The destination of an update/change, as shown by @git tree-diff@
-- data Dst = Dst { dstMode :: Mode
--                , dstSHA1 :: SHA1
--                , dstPath :: Maybe Path
--                } deriving (Eq, Ord, Show)

-- | A single @git tree-diff@ result
data DiffTree = DiffTree { src    :: DiffCommit
                         , dst    :: DiffCommit
                         , status :: Status
                         } deriving (Eq, Ord, Show)
------------------------------------------------------------------------------------------------------------------------------------------------------
-- | A percentage
newtype Percent = Percent Int deriving (Eq, Ord)

-- | Show a percentage with the @%@ sign
instance Show Percent where
  show (Percent p) = show p ++ "%"

-- | Parse a percentage
parsePercent :: Parser Percent
parsePercent = do
  p <- count 2 digit
  return . Percent . read $ p
------------------------------------------------------------------------------------------------------------------------------------------------------

-- | The `Status` of a diff, as shown by @git tree-diff@
data Status = Addition
            | Copy              Percent
            | Delete
            | Modify     (Maybe Percent)
            | Rename            Percent
            | TypeChange
            | Unmerged
            | Unknown    deriving (Eq, Ord, Show)

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
------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Parse a `DiffTree`
parseDiffTree :: Parser DiffTree
parseDiffTree = do
  _ <- char ':'
  srcMode' <- parseMode
  _ <- char ' '
  dstMode' <- parseMode
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

-- :100644 100644 ed57f1e415cbd5e7794372c0418ab713817176bc b7c37d458013415b4d08cd86aee55eb6b4113518 M	src/Data/Tree/Utils.hs
-- :100644 100644 8692f4d2b450940d516738d0f7ed8d0ef9a3c176 781238552012e599d6e3cfb5c8d6f665a5a8d45b M	src/Git/Commit.hs
-- :100644 100644 34b7a2b60f74da60478dd018c8b889728c808af9 3355d55a1af01ee3ea00ec389215bc4b3d9682fa M	src/Git/Commit/Parse.hs
-- :100644 100644 d5094914a87bbe7d2d586e016ecb533871526697 a71c2e79038d3e99e791c6bde925504dd9e069a7 M	src/Git/Details.hs
-- :100644 100644 1a0a1d325cb1b330ae390c43695beedd342b8c43 3aa20be59dff1800983536d3e36b9b175dfb8789 M	src/Git/DiffTree.hs


-- in-place edit  :100644 100644 bcd1234... 0123456... M file0
-- copy-edit      :100644 100644 abcd123... 1234567... C68 file1 file2
-- rename-edit    :100644 100644 abcd123... 1234567... R86 file1 file3
-- create         :000000 100644 0000000... 1234567... A file4
-- delete         :100644 000000 1234567... 0000000... D file5
-- unmerged       :000000 000000 0000000... 0000000... U file6
-- That is, from the left to the right:

-- a colon.
-- mode for "src"; 000000 if creation or unmerged.
-- a space.
-- mode for "dst"; 000000 if deletion or unmerged.
-- a space.
-- sha1 for "src"; 0{40} if creation or unmerged.
-- a space.
-- sha1 for "dst"; 0{40} if creation, unmerged or "look at work tree".
-- a space.
-- status, followed by optional "score" number.
-- path for "src"
-- path for "dst"; only exists for C or R.

-- Possible status letters are:
-- A: addition of a file
-- C: copy of a file into a new one
-- D: deletion of a file
-- M: modification of the contents or mode of a file
-- R: renaming of a file
-- T: change in the type of the file
-- U: file is unmerged (you must complete the merge before it can be committed)
-- X: "unknown" change type (most probably a bug, please report it)

-- Status letters C and R are always followed by a score (denoting the percentage of similarity between the source and target of the move or copy). Status letter M may be followed by a score (denoting the percentage of dissimilarity) for file rewrites.

-- <sha1> is shown as all 0’s if a file is new on the filesystem and it is out of sync with the index.

-- Example:

-- :100644 100644 5be4a4...... 000000...... M file.c
