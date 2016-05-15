module Git.Commit.Parse where

import Control.Applicative (many)
import Data.Attoparsec.Text (Parser, endOfInput, parseOnly, takeTill)
import Data.Attoparsec.Text.Utils (unsafeDecimal, skipChar, skipLine, takeTillEq)
import Data.Text (pack)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Git.Commit
import System.Process.Utils (simpleRun)



-- | This should match a date with the following form:
-- @2016-05-03 19:25:24 -0400\n@
parseDate :: Parser UTCTime
parseDate = do
  year   <- takeTillEq '-'
  skipChar
  month  <- takeTillEq '-'
  skipChar
  day    <- takeTillEq ' '
  skipChar
  hour   <- takeTillEq ':'
  skipChar
  minute <- takeTillEq ':'
  skipChar
  sec    <- takeTillEq ' '
  skipLine
  let d = fromGregorian (unsafeDecimal year) (unsafeDecimal month) (unsafeDecimal day)
  let t  = secondsToDiffTime $ (unsafeDecimal hour) * (60 * 60) + (unsafeDecimal minute) * (60) + (unsafeDecimal sec)
  return $ UTCTime { utctDay     = d
              , utctDayTime = t
              }

-- | Parse a single line returned from @git log@
parseLogLine :: Parser Commit
parseLogLine = do
  h <- takeTill (== '|')
  skipChar
  d <- parseDate
  return $ Commit { hash=h
             , date=d
             }

-- | This function gets all the `Commit`s for the current branch
-- An example of what this should parse:
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
getCommits :: IO (Either String [Commit])
getCommits = do
  maybeResults <- simpleRun "git" ["log", "--date=short", "--pretty=\"%H|%cd\""] ""
  case maybeResults of
    Left  err     -> return $ Left err
    Right results -> return . parseOnly (many $ parseLogLine <* endOfInput) . pack $ results

