{-|
Module      : Data.DepthElement
Description : A `DepthElement` is a `Text` object with 'depth' so that it can
              be unfolded into a `Tree`
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Text.Builder.Utils where


import Data.Char                     ( isSpace
                                     )
import Data.Monoid                   ( (<>)
                                     )
import qualified Data.Text.Lazy as L ( Text
                                     , map
                                     )
import Data.Text.Internal.Builder    ( Builder
                                     , fromLazyText
                                     , singleton
                                     , toLazyText
                                     )
import TextShow                      ( TextShow(..)
                                     )


-- | Map all non-whitespace characters to whitespace and leave whitespace alone
blankOutChar :: Char -> Char
blankOutChar c | isSpace c =  c
               | otherwise = ' '

-- | Apply `blankOutChar` to lazy `Text`
blankOut :: L.Text -> L.Text
blankOut = L.map blankOutChar

-- | `blankOut` "lifted" to `Builder`
blankOutBuilder :: Builder -> Builder
blankOutBuilder = bmap blankOut

-- | Convert a function on lazy text to one on `Builder`
bmap :: (L.Text -> L.Text) -> Builder -> Builder
bmap f = fromLazyText . f . toLazyText

-- | `unlines` combined with `showb` for `Builder`s
unlinesb :: TextShow a => [a] -> Builder
unlinesb = mconcat . map ((<> singleton '\n') . showb)

