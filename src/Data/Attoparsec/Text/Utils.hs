module Data.Attoparsec.Text.Utils where

import Control.Monad (replicateM_)
import Data.Attoparsec.Text (Parser, skip, takeTill, isEndOfLine)
import Data.Text (Text)
import Data.Text.Read (decimal)

-- | Skip any char
skipChar :: Parser ()
skipChar = skip (const True)

-- | Skip any @N@ chars
skipN :: Int -> Parser ()
skipN = flip replicateM_ skipChar

-- | Skip an entire line (up to and including the newline)
skipLine :: Parser ()
skipLine = do
  takeTill isEndOfLine
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
