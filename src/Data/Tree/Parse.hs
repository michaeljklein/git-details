{-# LANGUAGE OverloadedStrings #-}

module Data.Tree.Parse where

import Data.Tree (Tree(..), unfoldTree)
import Data.DepthElement
import Data.Text (Text)

import Data.Attoparsec.Text ( digit
                            , IResult(..)
                            , Parser
                            , Result
                            , endOfInput
                            , isEndOfLine
                            -- , many'
                            , many1
                            , manyTill
                            , parse
                            , skip
                            , skipSpace
                            , string
                            , takeText
                            , takeTill
                            )
import Data.Attoparsec.Text.Utils (takeLine)

data DirectoryTree = DirTree { numFiles :: Int
                             , numDirs  :: Int
                             , dirTree  :: Tree Text
                             } deriving (Eq, Show)

parseDirCounts :: Parser (Int, Int)
parseDirCounts = do
  dirs <- many1 digit
  _ <- string " directories, "
  files <- many1 digit
  _ <- string " files\n"
  return (read dirs, read files)

depthTree :: Parser (Tree Text)
depthTree = do
  dir <- takeLine
  es <- many1 parseDepthElement
  _ <- takeLine
  (dirs, files) <- parseDirCounts
  let splitted = initialSplit es
  let nonempty = filter (not . null) splitted
  return $ Node dir $ unfoldDepthTree nonempty
