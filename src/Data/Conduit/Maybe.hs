{-|
module      : Data.Conduit.Maybe
description : Conduits for Maybe types
copyright   : (c) michael klein, 2016
license     : bsd3
maintainer  : lambdamichael(at)gmail.com
-}

module Data.Conduit.Maybe where


import Data.Conduit             ( Conduit
                                )
import Data.Conduit.Combinators ( map
                                )
import Prelude hiding           ( map
                                )


-- | A `Conduit` that converts @Maybe Bool@ to @Bool@, taking @Just True@
-- as the only @True@ value
fromMaybeBool :: Monad m => Conduit (Maybe Bool) m Bool
fromMaybeBool = map fromMaybeBool'
  where
    fromMaybeBool' :: Maybe Bool -> Bool
    fromMaybeBool' (Just True) = True
    fromMaybeBool'  _          = False

