{-# LANGUAGE OverloadedStrings #-}

module Data.DepthElement where

import Data.Attoparsec.Text.Utils (takeLine)
import Control.Applicative (Alternative(..))
import Data.Text (Text)
import Data.Tree (Forest, unfoldForest)
import Data.Attoparsec.Text ( Parser
                            , many1
                            , string
                            )
import Data.List.Split (keepDelimsL, split, whenElt)

data DepthElement = DepthElement { depth :: Int
                                 , dElem :: Text
                                 } deriving (Show)

instance Eq DepthElement where
  (==) x y = (==) (depth x) (depth y)

-- | This is the character sequence for a middle branch
midBranch :: Parser Text
midBranch    = string "├── "

-- | This is the character sequence for left-padding a branch
preBranch :: Parser Text
preBranch    = string "│   "

-- | This is the character sequence for the last branch
endBranch :: Parser Text
endBranch    = string "└── "

-- | This is the sequence of spces for left-padding a branch
branchSpaces :: Parser Text
branchSpaces = string "    "

-- | Use the above branch parsers to get the depth of a branch in a tree
parseDepth :: Parser Int
parseDepth = do
  units <- many1 $ midBranch <|> preBranch <|> endBranch <|> branchSpaces
  return . length $ units

-- | Parse a complete line from a printed tree
parseDepthElement :: Parser DepthElement
parseDepthElement = do
  d <- parseDepth
  e <- takeLine
  return $ DepthElement d e

-- | Split a list of `DepthElement`s according to `depth`
splitOnDepth :: [DepthElement] -> [[DepthElement]]
splitOnDepth (x:xs) = split (keepDelimsL $ whenElt (== x)) (x:xs)
splitOnDepth  _     = []

-- | The unfolder used by `unfoldDepthTree`, treating the head of the list
-- as the root of the input tree implicitly
unfolder :: [DepthElement] -> (Text, [[DepthElement]])
unfolder (x:xs) = (dElem x, splitOnDepth xs)
unfolder  _     = (""     , []             )

-- | Begin splitting up the `DepthElement`s for unfolding
initialSplit :: [DepthElement] -> [[DepthElement]]
initialSplit = split $ keepDelimsL $ whenElt ((1 ==) . depth)

-- | Unfold a `[[DepthElement]]` into a forest
unfoldDepthTree :: [[DepthElement]] -> Forest Text
unfoldDepthTree = unfoldForest unfolder
