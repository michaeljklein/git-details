{-|
Module      : Data.Char.Parse
Description : Parse escaped characters
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Char.Parse where


import Data.Attoparsec.Text ( Parser
                            , anyChar
                            , char
                            )


-- | Parse a single escaped `Char` (fails on non-escaped)
escapedChar :: Parser Char
escapedChar = char '\\' >> anyChar >>= return . escapeChar

-- | Escape a `Char`, per
-- <https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/base-4.2.0.1/src/Text-Read-Lex.html Text.Read.Lex>
escapeChar :: Char -> Char
escapeChar c = case c of
                 'a' -> '\a'
                 'b' -> '\b'
                 'f' -> '\f'
                 'n' -> '\n'
                 'r' -> '\r'
                 't' -> '\t'
                 'v' -> '\v'
                 '\\' -> '\\'
                 '\"' -> '\"'
                 '\'' -> '\''
                 _  -> c

