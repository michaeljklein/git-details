{-# LANGUAGE OverloadedStrings #-}

module Data.DepthElement where

import Data.Attoparsec.Text.Utils (takeLine)
import Control.Applicative (Alternative(..))
import Data.Text (Text)
import Data.Tree (Forest, unfoldForest)
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
import Data.List.Split (Splitter, keepDelimsL, split, whenElt)

data DepthElement = DepthElement { depth :: Int
                                 , dElem :: Text
                                 } deriving (Show)

instance Eq DepthElement where
  (==) x y = (==) (depth x) (depth y)

midBranch :: Parser Text
midBranch    = string "├── "

preBranch :: Parser Text
preBranch    = string "│   "

endBranch :: Parser Text
endBranch    = string "└── "

branchSpaces :: Parser Text
branchSpaces = string "    "

parseDepth :: Parser Int
parseDepth = do
  units <- many1 $ midBranch <|> preBranch <|> endBranch <|> branchSpaces
  return . length $ units


parseDepthElement :: Parser DepthElement
parseDepthElement = do
  depth <- parseDepth
  element <- takeLine
  return $ DepthElement depth element


splitOnDepth :: [DepthElement] -> [[DepthElement]]
splitOnDepth (x:xs) = split (keepDelimsL $ whenElt (== x)) (x:xs)
splitOnDepth  _     = []

unfolder :: [DepthElement] -> (Text, [[DepthElement]])
unfolder (x:xs) = (dElem x, splitOnDepth xs)
unfolder  _     = (""     , []             )

initialSplit :: [DepthElement] -> [[DepthElement]]
initialSplit = split $ keepDelimsL $ whenElt ((1 ==) . depth)

unfoldDepthTree :: [[DepthElement]] -> Forest Text
unfoldDepthTree = unfoldForest unfolder
