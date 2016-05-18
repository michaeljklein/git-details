module Git.Types where


import Data.Text ( Text
                 , singleton
                 , replicate
                 )

import Data.Time.Clock (UTCTime(..))
import Prelude hiding (replicate)

-- | `Commit` only contains the hash and date of the commit
data Commit = Commit { hash :: SHA1
                     , date :: UTCTime
                     } deriving (Eq)

-- | As I'm not sure whether the hashes are ordered according to the dates,
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


-- | A mode is up to 6 digits and is found in the @git tree-diff@
-- command results
data Mode = Mode Int deriving (Eq, Ord, Show)

-- | `Mode` of a created object
creationMode :: Mode
creationMode = Mode 0

-- | `Mode` of an unmerged object
unmergedMode :: Mode
unmergedMode = creationMode


-- | A filepath, relative to the root directory of the project
type Path = Text

