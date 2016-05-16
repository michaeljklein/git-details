{-|
Module      : Data.Tree.Utils
Description : Several utilities for manipulating `Tree`s
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com

`TextShow` instances for `Tree`s.
Several general and specialized filters for `Tree`s.
`Tree` and `Forest` union(by).
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.Tree.Utils ( forestBranches
                       , forestFilter
                       , forestLeaves
                       , forestSort
                       , forestSortBy
                       , forestUnion
                       , forestUnionBy
                       , treeBranches
                       , treeFilter
                       , treeLeaves
                       , treeSort
                       , treeSortBy
                       , treeUnion
                       , treeUnionBy
                       ) where


import Data.DepthElement       ( baseDepth
                               , forestDepthElements
                               , treeDepthElements
                               )
import Data.List               ( sortBy
                               )
import Data.List.Utils         ( keepPartition
                               )
import Data.Text.Builder.Utils ( unlinesb
                               )
import Data.Tree               ( Forest
                               , Tree(..)
                               )
import TextShow                ( TextShow(..)
                               )


instance TextShow a => TextShow (Tree a) where
  showb = unlinesb . treeDepthElements   baseDepth

instance TextShow a => TextShow (Forest a) where
  showb = unlinesb . forestDepthElements baseDepth


-- | @forestFilter p forest@ returns a list of all the subtrees in @forest@
-- that satisfy @p@
forestFilter :: (Tree a -> Bool) -> Forest a -> [Tree a]
forestFilter _ [] = []
forestFilter p x  = keep ++ forestFilter p (bs >>= subForest)
  where
    ~(keep, _, bs) = keepPartition p (null . subForest) x

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

-- | Note that all these sorts can be tested by the following:
-- sort   . sort    == sort
-- sort   . reverse == id
-- length . sort    == length
treeSort :: Ord a => Tree a -> Tree a
treeSort = treeSortBy compare

-- | This is `treeSort`, except it uses a user-supplied comparison function
-- instead of the overloaded `compare` function.
treeSortBy :: (a -> a -> Ordering) -> Tree a -> Tree a
treeSortBy cmp (Node x xs) = Node x $ forestSortBy cmp xs

forestSort :: Ord a => Forest a -> Forest a
forestSort = forestSortBy compare

-- | This is `forestSort`, except it uses a user-supplied comparison function
-- instead of the overloaded `compare` function.
forestSortBy :: (a -> a -> Ordering) -> Forest a -> Forest a
forestSortBy cmp = map (treeSortBy cmp) . sortBy cmp'
  where
    cmp' x y = cmp (rootLabel x) (rootLabel y)

-- | Why require `Ord` for `treeUnion` and related functions?
-- It allows roughly @O(n)@ time operations.
--
-- >>> treeUnion x x
-- [x]
--
-- If @x /= y@, then
--
-- >>> treeUnion (Node x a) (Node y b)
-- [Node x a, Node y a]
--
-- >>> treeUnion (Node 1 [Node 2 []]) (Node 1 [Node 2 [], Node 3 [])
-- [Node 1 [Node 2 [], Node 3 []]]
--
treeUnion :: Ord a => Tree a -> Tree a -> Forest a
treeUnion = treeUnionBy compare

-- | This is `treeUnion`, except it uses a user-supplied comparison function
-- instead of the overloaded `compare` function.
treeUnionBy :: (a -> a -> Ordering) -> Tree a -> Tree a -> Forest a
treeUnionBy cmp x y = forestUnionBy cmp [x] [y]

-- | This effectively applies `treeUnion` to all pairs of `Tree`s in the
-- given forests, until there is no further change
forestUnion   :: Ord a => Forest a -> Forest a -> Forest a
forestUnion = forestUnionBy compare

-- | This is `forestUnion`, except it uses a user-supplied comparison function
-- instead of the overloaded `compare` function.
forestUnionBy :: (a -> a -> Ordering) -> Forest a -> Forest a -> Forest a
forestUnionBy cmp xs ys = forestUnionBy' cmp xs' ys'
  where
    xs' = forestSortBy cmp xs
    ys' = forestSortBy cmp ys

-- | This function requires that its arguments are sorted, otherwise it is
-- equivalent to `forestUnionBy`
forestUnionBy' :: (a -> a -> Ordering) -> Forest a -> Forest a -> Forest a
forestUnionBy' _     xs    []    =                  xs
forestUnionBy' _   []        ys  =                     ys
forestUnionBy' c s t = case c (rootLabel x) (rootLabel y) of
                         LT -> x : forestUnionBy' c xs t
                         GT -> y : forestUnionBy' c s  ys
                         EQ -> u : forestUnionBy' c xs ys
  where
    ~(x:xs) = s
    ~(y:ys) = t
    ~[u]    = treeUnionBy c x y

