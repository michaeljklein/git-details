module Git.Types.Parse where


import Control.Applicative        ( (<|>)
                                  )
import Control.Monad              ( liftM
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , anyChar
                                  , char
                                  , count
                                  , digit
                                  , many'
                                  , many1
                                  , skipSpace
                                  , takeWhile1
                                  , takeTill
                                  )
import Data.Attoparsec.Text.Utils ( failParse
                                  , maybeParse
                                  , takeLine
                                  , skipChar
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

import Data.Time.Clock.Parse      ( parseDate
                                  )
import Git.Types



-- | Parse a `SHA1` object
parseSHA1 :: Parser SHA1
parseSHA1 = count 40 anyChar >>= return . SHA1 . pack

-- | Parse a `Path`. This will escape characters in the path,
-- but may otherwise not be up to all (POSIX, Windows, etc.)
-- path specifications
parsePath :: Parser Path
parsePath = liftM pack . many1 $ escapedChar <|> anyChar

-- | Parse a `Mode`
parseMode :: Parser Mode
parseMode = count 6 digit >>= return . Mode . read

-- | Parse a single line returned from @git log@
parseLogLine :: Parser Commit
parseLogLine = do
  h <- parseSHA1
  skipChar
  d <- parseDate
  return $ Commit { hash=h
             , date=d
             }
