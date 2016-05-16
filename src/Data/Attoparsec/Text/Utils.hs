{-|
Module      : Data.Attoparsec.Text.Utils
Description : Utilities to extend the functionality of Data.Attoparsec.Text
 Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Attoparsec.Text.Utils where

import Control.Monad        ( replicateM_
                            )
import Data.Attoparsec.Text ( Parser
                            , isEndOfLine
                            , satisfyWith
                            , skip
                            , takeTill
                            )
import Data.Text            ( Text
                            )
import Data.Text.Read       ( decimal
                            )


-- | Skip any char
skipChar :: Parser ()
skipChar = skip (const True)

-- | Skip any @N@ chars
skipN :: Int -> Parser ()
skipN = flip replicateM_ skipChar

-- | Skip an entire line (up to and including the newline)
skipLine :: Parser ()
skipLine = do
  _ <- takeTill isEndOfLine
  skipChar

-- | Take an entire line (up to, but not including, the newline)
takeLine :: Parser Text
takeLine = do
  result <- takeTill isEndOfLine
  skipChar
  return result

-- | Take until equal
takeTillEq :: Char -> Parser Text
takeTillEq = takeTill . (==)

-- | Returns @0@ on failed parse
unsafeDecimal :: Integral a => Text -> a
unsafeDecimal x = case decimal x of
                    Right (y, _) -> y
                    Left _       -> 0

-- | Fail a parse unconditionally
failParse :: Parser a
failParse = satisfyWith (const undefined) (const False)

