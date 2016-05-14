module Git.Commit where

import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))


-- | `Commit` only contains the hash and date of the commit
data Commit = Commit { hash :: Text
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

