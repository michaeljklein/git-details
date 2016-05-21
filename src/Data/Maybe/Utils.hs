{-|
module      : Data.Maybe.Utils
description : Utility functions for "Data.Maybe"
copyright   : (c) michael klein, 2016
license     : bsd3
maintainer  : lambdamichael(at)gmail.com
-}

module Data.Maybe.Utils where


-- | @justIfTrue b x == if b then Just x else Nothing@
justIfTrue :: Bool -> a -> Maybe a
justIfTrue True x = Just x
justIfTrue _    _ = Nothing

