{-|
module      : Data.Conduit.Lens
description : Lens Conduits
copyright   : (c) michael klein, 2016
license     : bsd3
maintainer  : lambdamichael(at)gmail.com
-}


module Data.Conduit.Lens ( getterC ) where


import Control.Lens.Getter      ( Getting
                                , (^.)
                                )
import Data.Conduit             ( Conduit
                                )
import Data.Conduit.Combinators ( map
                                )
import Prelude hiding           ( map
                                )


-- | Given a getter @g@, @getterConduit g@ takes what @g@ gets from as input and
-- outputs what is gotten by @g@. For example,
-- @getterConduit _1 :: Conduit (a,b) m a@
getterC :: Monad m => Getting a s a -> Conduit s m a
getterC g = map (^. g)

