{-|
Module      : Data.Opaque
Description : `Opaque` elements can be transparent, i.e. are whitespace when shown
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}


module Data.Opaque where


import TextShow                       ( TextShow(..)
                                      )
import Data.Text.Builder.Utils        ( blankOutBuilder
                                      )
import Data.Tree                      ( Forest
                                      , Tree
                                      )


-- | An element that can be opaque (visible when shown) or transparent
data Opaque a = Opaque a
              | Transparent a
              deriving (Eq, Ord, Show)

-- | When a transparent object is shown, all its non-whitespace characters are
-- replaced with spaces (to preserve formatting, yet still hide)
instance TextShow a => TextShow (Opaque a) where
  showb (Opaque      a) =                   showb $ a
  showb (Transparent a) = blankOutBuilder . showb $ a

-- | A `Tree` of `Opaque` elements
type OpaqueTree   a = Tree   (Opaque a)

-- | A `Forest` of `Opaque` elements
type OpaqueForest a = Forest (Opaque a)


