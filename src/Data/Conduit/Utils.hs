{-|
module      : Data.Conduit.Utils
description : Utilities for general Conduits
copyright   : (c) michael klein, 2016
license     : bsd3
maintainer  : lambdamichael(at)gmail.com
-}

module Data.Conduit.Utils where


import Control.Concurrent.STM.TQueue ( newTQueueIO
                                     )
import Control.Monad.IO.Class        ( MonadIO
                                     )
import Conduit                       ( mapC
                                     , mapMC
                                     , mapM_C
                                     , yieldMany
                                     )
import Data.Conduit                  ( Conduit
                                     , Source
                                     , ($$)
                                     , (=$=)
                                     , await
                                     )
import Data.Conduit.TQueue           ( sinkTQueue
                                     , sourceTQueue
                                     )
import Data.Conduit.Internal         ( zipSources
                                     )
import Data.MonoTraversable          ( Element
                                     , MonoFoldable)


-- | `await` for a value from each `Source`, outputting `Nothing` when the
-- `Source` is empty
awaitC :: Monad m => Conduit (Source m a) m (Maybe a)
awaitC = mapMC ($$ await)

-- | `fuse`, inside of a `Conduit`
fusingC :: (Monad m, Monad n) => Conduit i m o -> Conduit (Source m i) n (Source m o)
fusingC c = mapC (=$= c)

-- | Given a `Source` and two `Conduit`s, zip the `Conduits` together, applied
-- to the `Source`, to make a new `Source`
zipToSource :: Monad m => Source m o -> Conduit o m a -> Conduit o m b -> Source m (a, b)
zipToSource = zipToSourceWith $ curry id

-- | Like `zipToSource`, but zipping together with a different function
-- than `(,)`
zipToSourceWith :: Monad m => (a -> b -> c) -> Source m o -> Conduit o m a -> Conduit o m b -> Source m c
zipToSourceWith f s c1 c2 = zipSources (s =$= c1) (s =$= c2) =$= mapC (uncurry f)

elementS :: (MonadIO m, MonoFoldable mono) => Conduit () IO mono -> IO (Source m (Element mono))
elementS s = do
  queue <- newTQueueIO
  let sink = sinkTQueue queue
  s =$= mapC yieldMany $$ mapM_C ($$ sink)
  return $ sourceTQueue queue
