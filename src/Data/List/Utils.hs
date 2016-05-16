{-|
Module      : Data.List.Utils
Description : A few utilities for working with lists
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com

The two tools here are straightforward extensions to
what's found in "Data.List"
-}

module Data.List.Utils where


-- | @keepPartition keep pred x == (filter keep x, filter pred x, filter (not . pred) x)@
keepPartition :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
{-# INLINE keepPartition #-}
keepPartition k p xs = foldr (keepSelect k p) ([], [], []) xs

-- | `select` extended to allow not only partitioning, but also keeping some
-- values. See `keepPartition`
keepSelect :: (a -> Bool) -> (a -> Bool) -> a -> ([a], [a], [a]) -> ([a], [a], [a])
keepSelect k p x ~(ks, ts, fs) = if k x
                                    then if p x
                                            then (x:ks, x:ts,   fs)
                                            else (x:ks,   ts, x:fs)
                                    else if p x
                                            then (  ks, x:ts,   fs)
                                            else (  ks,   ts, x:fs)

