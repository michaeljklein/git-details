{-|
Module      : Data.Time.Clock.Parse
Description : Parse `UTCTime` of a certain format
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}


module Data.Time.Clock.Parse where


import Data.Attoparsec.Text       ( Parser
                                  , digit
                                  )
import Data.Attoparsec.Text.Utils ( skipChar
                                  , skipLine
                                  , takeTillEq
                                  )
import Data.Time.Calendar         ( fromGregorian
                                  )
import Data.Time.Clock            ( UTCTime(..)
                                  , secondsToDiffTime
                                  )


-- | This should match a date with the following form:
-- @2016-05-03 19:25:24 -0400\n@
parseDate :: Parser UTCTime
parseDate = do
  year   <- digit `takeTillEq` '-'
  skipChar
  month  <- digit `takeTillEq` '-'
  skipChar
  day    <- digit `takeTillEq` ' '
  skipChar
  hour   <- digit `takeTillEq` ':'
  skipChar
  minute <- digit `takeTillEq` ':'
  skipChar
  sec    <- digit `takeTillEq` ' '
  skipLine
  let d = fromGregorian (read year) (read month) (read day)
  let t = secondsToDiffTime $ (read hour) * (60 * 60) + (read minute) * (60) + (read sec)
  return $ UTCTime { utctDay     = d
              , utctDayTime = t
              }

