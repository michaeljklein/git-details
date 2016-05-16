{-|
Module      : Git.TreeDiff
Description : Types and parsers for the results of @git diff-tree@
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Git.TreeDiff where

import Control.Applicative        ( (<|>)
                                  )
import Control.Monad              ( liftM
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , anyChar
                                  , char
                                  , count
                                  , digit
                                  , many1
                                  , skipSpace
                                  , takeWhile1
                                  )
import Data.Attoparsec.Text.Utils ( failParse
                                  , maybeParse
                                  )
import Data.Char                  ( isSpace
                                  )
import Data.Char.Parse            ( escapedChar
                                  )
import Data.Text                  ( Text
                                  , pack
                                  , replicate
                                  , singleton
                                  )

import Prelude hiding             ( replicate
                                  )

-- | `SHA1` objects should contain exactly 40 characters and
-- corresponds to the digest of the hash
newtype SHA1 = SHA1 Text deriving (Eq, Ord, Show)

-- | Parse a `SHA1` object
parseSHA1 :: Parser SHA1
parseSHA1 = count 40 anyChar >>= return . SHA1 . pack

-- | A mode is up to 6 digits and is found in the @git tree-diff@
-- command results
data Mode = Mode Int deriving (Eq, Ord, Show)

-- | Parse a `Mode`
parseMode :: Parser Mode
parseMode = count 6 digit >>= return . Mode . read

-- | A filepath, relative to the root directory of the project
type Path = Text

-- | Parse a `Path`. This will escape characters in the path,
-- but may otherwise not be up to all (POSIX, Windows, etc.)
-- path specifications
parsePath :: Parser Path
parsePath = liftM pack . many1 $ escapedChar <|> anyChar


------------------------------------------------------------------------------------------------------------------------------------------------------

-- | The source of an update/change, as shown by @git tree-diff@
data Src = Src { srcMode :: Mode
               , srcSHA1 :: SHA1
               , srcPath :: Path
               } deriving (Eq, Ord, Show)

-- | The destination of an update/change, as shown by @git tree-diff@
data Dst = Dst { dstMode :: Mode
               , dstSHA1 :: SHA1
               , dstPath :: Maybe Path
               } deriving (Eq, Ord, Show)

-- | A single @git tree-diff@ result
data TreeDiff = TreeDiff { src    :: Src
                         , dst    :: Dst
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

-- | `Mode` of a created object
creationMode :: Mode
creationMode = Mode 0

-- | `Mode` of an unmerged object
unmergedMode :: Mode
unmergedMode = creationMode

-- | `SHA` of a created object
creationSHA1 :: SHA1
creationSHA1 = SHA1 $ replicate 40 $ singleton '0'

-- | `SHA1` of an unmerged object
unmergedSHA1 :: SHA1
unmergedSHA1 = creationSHA1

-- | Git documentation says that this is the `SHA1` representing
-- the message "look at work tree".
lookAtTreeSHA1 :: SHA1
lookAtTreeSHA1 = creationSHA1

-- | Accepts any non-empty, spaceless text
parseSpaceless :: Parser Text
parseSpaceless = takeWhile1 $ not . isSpace

-- | Parse a `TreeDiff`
parseTreeDiff :: Parser TreeDiff
parseTreeDiff = do
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
  let src' = Src srcMode' srcSHA1' srcPath'
  let dst' = Dst dstMode' dstSHA1' dstPath'
  return $ TreeDiff src' dst' status'




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
