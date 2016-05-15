{-# LANGUAGE OverloadedStrings #-}

module Data.Tree.Parse where

import Data.Attoparsec.Text ( digit
                            , Parser
                            , many1
                            , string
                            )
import Data.Attoparsec.Text.Utils (takeLine)
import Data.DepthElement
import Data.Text (Text)
import Data.Tree (Tree(..))


-- | This is a data type for a directory tree, with added details on the number
-- of files, directories
data DirectoryTree = DirTree { numFiles :: Int        -- ^ The number of files in the given directory
                             , numDirs  :: Int        -- ^ The number of subdirectories in the given directory
                             , dirTree  :: Tree Text  -- ^ The directory tree
                             } deriving (Eq, Show)

-- | Parse the directory (and file) counts returned by the @tree@ command
parseDirCounts :: Parser (Int, Int)
parseDirCounts = do
  dirs <- many1 digit
  _ <- string " directories, "
  files <- many1 digit
  _ <- string " files\n"
  return (read dirs, read files)

-- | Parse a directory tree (from what's returned by the @tree@ command
parseDirectoryTree :: Parser DirectoryTree
parseDirectoryTree = do
  dir <- takeLine
  es <- many1 parseDepthElement
  _ <- takeLine
  (dirs, files) <- parseDirCounts
  let splitted = initialSplit es
  let nonempty = filter (not . null) splitted
  return $ DirTree files dirs $ Node dir $ unfoldDepthTree nonempty
