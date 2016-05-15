module Git.DiffTree where

import Control.Applicative        ( (<|>)
                                  )
import Control.Monad              ( liftM
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , char
                                  , count
                                  , digit
                                  , takeWhile1
                                  )
import Data.Attoparsec.Text.Utils ( failParse
                                  )
import Data.Char                  ( isSpace
                                  )
import Data.Text                  ( Text
                                  , replicate
                                  , singleton
                                  )

import Prelude hiding             ( replicate
                                  )


newtype SHA1 = SHA1 Text deriving (Eq, Ord, Show)

data Mode = Mode Int deriving (Eq, Ord, Show)

type Path = Text

------------------------------------------------------------------------------------------------------------------------------------------------------
data Src = Src { srcMode :: Mode
               , srcSHA1 :: SHA1
               , srcPath :: Path
               } deriving (Eq, Ord, Show)

data Dst = Dst { dstMode :: Mode
               , dstSHA1 :: SHA1
               , dstPath :: Maybe Path
               } deriving (Eq, Ord, Show)

data DiffTree = DiffTree { src    :: Src
                         , dst    :: Dst
                         , status :: Status
                         } deriving (Eq, Ord, Show)
------------------------------------------------------------------------------------------------------------------------------------------------------
-- | A percentage
newtype Percent = Percent Int deriving (Eq, Ord)

instance Show Percent where
  show (Percent p) = show p ++ "%"

-- | Parse a percentage
parsePercent :: Parser Percent
parsePercent = do
  p <- count 2 digit
  return . Percent . read $ p
------------------------------------------------------------------------------------------------------------------------------------------------------
data Status = Addition
            | Copy              Percent
            | Delete
            | Modify     (Maybe Percent)
            | Rename            Percent
            | TypeChange
            | Unmerged
            | Unknown    deriving (Eq, Ord, Show)

parseOneStatus :: Char -> Status -> Parser Status
parseOneStatus c s = char c >> return s

parseModifyStatus :: Char -> (Maybe Percent -> Status) -> Parser Status
parseModifyStatus 'M' _ = char 'M' >> liftM (Modify . Just) parsePercent <|> return (Modify Nothing)
parseModifyStatus  _  _ = failParse -- 'M' must be passed to parseModifyStatus

parseOnePercentStatus :: Char -> (Percent -> Status) -> Parser Status
parseOnePercentStatus 'C' _ = char 'C' >> liftM Copy   parsePercent
parseOnePercentStatus 'R' _ = char 'R' >> liftM Rename parsePercent
parseOnePercentStatus  _  _ = failParse -- Char other than 'C'/'R' passed to parseOnePercentStatus

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
