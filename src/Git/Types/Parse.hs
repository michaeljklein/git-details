module Git.Types.Parse where


import Control.Applicative        ( (<|>)
                                  )
import Control.Monad              ( liftM
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , anyChar
                                  , count
                                  , digit
                                  , many1
                                  )
import Data.Attoparsec.Text.Utils ( skipChar
                                  )
import Data.Char.Parse            ( escapedChar
                                  )
import Data.Text                  ( pack
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