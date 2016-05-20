{-|
Module      : Git.Types
Description : Git types
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}


module Git.Types (
    -- * Commits
    Commit(..)
    -- * SHA1s
    , SHA1(..)
    , creationSHA1
    , unmergedSHA1
    , lookAtTreeSHA1
    -- * Modes
    , DiffMode(..)
    , creationMode
    , unmergedMode
    -- * Paths
    , Path(..)
    ) where


import Data.Text       ( Text
                       , singleton
                       , replicate
                       )
import Data.Time.Clock ( UTCTime(..)
                       )
import Prelude hiding  ( replicate
                       )


-- | `Commit` only contains the hash and date of the commit
data Commit = Commit { hash :: SHA1
                     , date :: UTCTime
                     } deriving (Eq)

-- | As the hashes may not be ordered according to the dates,
-- this disambiguates the order. If there are any errors in this method,
-- they should be apparent when looking at the resulting graphs
instance Ord Commit where
  compare c1 c2 = case compare (date c1) (date c2) of
                    LT -> LT
                    GT -> GT
                    EQ -> compare (hash c1) (hash c2)


-- | `SHA1` objects should contain exactly 40 characters and
-- corresponds to the digest of the hash
newtype SHA1 = SHA1 Text deriving (Eq, Ord, Show)

-- | `SHA1` of a created object
creationSHA1 :: SHA1
creationSHA1 = SHA1 $ replicate 40 $ singleton '0'

-- | `SHA1` of an unmerged object
unmergedSHA1 :: SHA1
unmergedSHA1 = creationSHA1

-- | Git documentation says that this is the `SHA1` representing
-- the message "look at work tree".
lookAtTreeSHA1 :: SHA1
lookAtTreeSHA1 = creationSHA1


-- | A `DiffMode` is up to 6 digits and is found in the @git tree-diff@
-- command results
data DiffMode = DiffMode Int deriving (Eq, Ord, Show)

-- | `DiffMode` of a created object
creationMode :: DiffMode
creationMode = DiffMode 0

-- | `DiffMode` of an unmerged object
unmergedMode :: DiffMode
unmergedMode = creationMode


-- | A filepath, relative to the root directory of the project
type Path = Text

