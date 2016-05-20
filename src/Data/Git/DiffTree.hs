{-|
Module      : Git.DiffTree
Description : Types for the results of @git diff-tree@
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Git.DiffTree where

import Git.Types                  ( DiffMode
                                  , Path
                                  , SHA1(..)
                                  )


-- | The source or destination of an update/change, as shown by @git tree-diff@
data DiffCommit = DiffCommit { diffMode :: DiffMode
                             , diffSHA1 :: SHA1
                             , diffPath :: Maybe Path
                             } deriving (Eq, Ord, Show)

-- | A single @git tree-diff@ result
data DiffTree = DiffTree { src    :: DiffCommit
                         , dst    :: DiffCommit
                         , status :: Status
                         } deriving (Eq, Ord, Show)


-- | A percentage
newtype Percent = Percent Int deriving (Eq, Ord)

-- | Show a percentage with the @%@ sign
instance Show Percent where
  show (Percent p) = show p ++ "%"


-- | The `Status` of a diff, as shown by @git tree-diff@
data Status = Addition
            | Copy              Percent
            | Delete
            | Modify     (Maybe Percent)
            | Rename            Percent
            | TypeChange
            | Unmerged
            | Unknown    deriving (Eq, Ord, Show)

