module Data.Tree.Utils ( forestBranches
                       , forestFilter
                       , forestLeaves
                       , treeBranches
                       , treeFilter
                       , treeLeaves
                       ) where

import Data.List.Utils (keepPartition, keepSelect)
import Data.Tree       (Tree(..), Forest)


-- | @forestFilter p forest@ returns a list of all the subtrees in @forest@
-- that satisfy @p@
forestFilter :: (Tree a -> Bool) -> Forest a -> [Tree a]
forestFilter _ [] = []
forestFilter p x  = keep ++ forestFilter p (branches >>= subForest)
  where
    ~(keep, leaves, branches) = keepPartition p (null . subForest) x

-- | `forestFilter` for `Tree`s
treeFilter :: (Tree a -> Bool) -> Tree a -> [Tree a]
treeFilter p x = forestFilter p [x]

-- | A binding function to allow using @forestFilter@/@treeFilter@ to
-- select either branches or leaves
nodeFilter :: (Bool -> c) -> ((Tree a1 -> c) -> a -> [Tree b]) -> a -> [b]
nodeFilter p f = map rootLabel . f (p . null . subForest)

-- | Given a tree/forest filter and a tree/forest, return all leaves
leaves   :: ((Tree a1 -> Bool) -> a -> [Tree b]) -> a -> [b]
leaves   = nodeFilter not

-- | Given a tree/forest filter and a tree/forest, return all branches
branches :: ((Tree a1 -> Bool) -> a -> [Tree b]) -> a -> [b]
branches = nodeFilter id

-- | Return a list of the `rootLabel`s of all leaves in a forest
forestLeaves   :: Forest a -> [a]
forestLeaves   = leaves   forestFilter

-- | Return a list of the `rootLabel`s of all branches in a forest
forestBranches :: Forest a -> [a]
forestBranches = branches forestFilter

-- | Return a list of the `rootLabel`s of all leaves in a tree
treeLeaves     :: Tree a -> [a]
treeLeaves     = leaves   treeFilter

-- | Return a list of the `rootLabel`s of all branches in a tree
treeBranches   :: Tree a -> [a]
treeBranches   = branches treeFilter

