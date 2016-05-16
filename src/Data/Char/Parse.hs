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


escapedChar :: Parser Char
escapedChar = char '\\' >> anyChar >>= return . escapeChar

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

