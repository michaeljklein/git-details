{-|
Module      : Data.DepthElement
Description : A `DepthElement` is a `Text` object with 'depth' so that it can
              be unfolded into a `Tree`
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.DepthElement ( Depth(..)
                         , DepthElement(..)
                         , DepthElemShow(..)
                         , baseDepth
                         , parseDepthElement
                         , initialSplit
                         , splitOnDepth
                         , forestDepthElements
                         , treeDepthElements
                         , unfoldDepthTree
                         ) where


import Control.Applicative        ( Alternative(..)
                                  )
import Data.Monoid                ( (<>)
                                  )
import Data.Text                  ( Text
                                  )
import Data.Text.Builder.Utils    ( unlinesb
                                  )
import Data.Tree                  ( Forest
                                  , Tree(..)
                                  , unfoldForest
                                  )
import Data.Attoparsec.Text       ( Parser
                                  , many1
                                  , string
                                  )
import Data.Attoparsec.Text.Utils ( takeLine
                                  )
import Data.List.Split            ( keepDelimsL
                                  , split
                                  , whenElt
                                  )
import TextShow                   ( TextShow(..)
                                  , fromText
                                  )


-- | This is the depth of an element in a `Tree`
-- If `Nat` were faster, this would use it instead of `Int`
newtype Depth = Depth Int deriving (Enum, Eq, Ord)

-- | The lowest valid depth (@0@)
baseDepth :: Depth
baseDepth = Depth 0

-- | This is a _very_ approximate inverse to the parser
instance TextShow Depth where
  showb (Depth 0) =                     fromText "├── "
  showb (Depth d) = mconcat $ spaces ++ [fromText "├── "]
    where
      spaces = replicate (d - 1) $ fromText "    "

-- | An element with depth
data DepthElement a = DepthElem { depth :: Depth
                                , dElem :: a
                                }

instance Eq (DepthElement a) where
  (==) x y = (==) (depth x) (depth y)

instance TextShow a => TextShow (DepthElement a) where
  showb (DepthElem d x) = showb d <> showb x


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
parseDepthElement :: Parser (DepthElement Text)
parseDepthElement = do
  d <- parseDepth
  e <- takeLine
  return $ DepthElem (Depth d) e

-- | Split a list of `DepthElement`s according to `depth`
splitOnDepth :: [DepthElement a] -> [[DepthElement a]]
splitOnDepth (x:xs) = split (keepDelimsL $ whenElt (== x)) (x:xs)
splitOnDepth  _     = []

-- | The unfolder used by `unfoldDepthTree`, treating the head of the list
-- as the root of the input tree implicitly
unfolder :: [DepthElement Text] -> (Text, [[DepthElement Text]])
unfolder (x:xs) = (dElem x, splitOnDepth xs)
unfolder  _     = (""     , []             )

-- | Begin splitting up the `DepthElement`s for unfolding
initialSplit :: [DepthElement a] -> [[DepthElement a]]
initialSplit = split $ keepDelimsL $ whenElt ((Depth 1 ==) . depth)

-- | Unfold `DepthElement` into a forest
unfoldDepthTree :: [[DepthElement Text]] -> Forest Text
unfoldDepthTree = unfoldForest unfolder


class ToDepthElements t where
  type ToDepthElem t
  toDepthElements      ::          t -> [DepthElement (ToDepthElem t)]
  depthToDepthElements :: Depth -> t -> [DepthElement (ToDepthElem t)]
  toDepthElements = depthToDepthElements baseDepth
  {-# MINIMAL depthToDepthElements #-}

instance ToDepthElements (Forest a) where
  type ToDepthElem (Forest a) = a
  depthToDepthElements = forestDepthElements

instance (a ~ a) => ToDepthElements (Tree a) where
  type ToDepthElem (Tree a) = a
  depthToDepthElements = treeDepthElements

-- | To make the `TextShow` instance not an orphan
newtype DepthElemShow a = DepthElemShow { _unDepthElemShow :: a}

instance (TextShow (ToDepthElem t), ToDepthElements t) => TextShow (DepthElemShow t) where
  showb = unlinesb . toDepthElements . _unDepthElemShow

-- | Convert a forest to a list of `DepthElement`s, given an initial depth
forestDepthElements :: Depth -> Forest a -> [DepthElement a]
forestDepthElements d = (>>= treeDepthElements d)

-- | `forestDepthElements` for `Tree`s
treeDepthElements :: Depth -> Tree a -> [DepthElement a]
treeDepthElements d ~(Node l s) = DepthElem d l : forestDepthElements (succ d) s

