{-|
Module      : Git.Types.Parse
Description : Parse Git types
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}


module Git.Types.Parse ( parseLogLine
                       , parseSHA1
                       , parseDiffMode
                       , parsePath
                       ) where


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


-- | Parse a single line returned from @git log@
parseLogLine :: Parser Commit
parseLogLine = do
  h <- parseSHA1
  skipChar
  d <- parseDate
  return $ Commit { hash=h
             , date=d
             }

-- | Parse a `SHA1` object
parseSHA1 :: Parser SHA1
parseSHA1 = count 40 anyChar >>= return . SHA1 . pack

-- | Parse a `Mode`
parseDiffMode :: Parser DiffMode
parseDiffMode = count 6 digit >>= return . DiffMode . read

-- | Parse a `Path`. This will escape characters in the path,
-- but may otherwise not be up to all (POSIX, Windows, etc.)
-- path specifications
parsePath :: Parser Path
parsePath = liftM pack . many1 $ escapedChar <|> anyChar

