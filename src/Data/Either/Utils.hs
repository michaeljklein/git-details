{-|
Module      : Data.Either.Utils
Description : Utilities for operating on the `Either` datatype
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Either.Utils where


-- | See the type. Nothing becomes @Left ()@
maybeToEither :: Maybe a -> Either () a
maybeToEither (Just x) = Right x
maybeToEither       _  = Left ()

maybeTupEither :: (Maybe a, b) -> Either a b
maybeTupEither  (Just x, _) = Left  x
maybeTupEither ~(     _, y) = Right y

-- | Swaps `Left` and `Right`
eitherSwap :: Either a b -> Either b a
eitherSwap (Left  x) = Right x
eitherSwap (Right x) = Left  x

-- | See the type. Lazy matches on @Right (Right x) -> Right x@
assocL :: Either a (Either b c) -> Either (Either a b) c
assocL  (       Left  x)  = Left $ Left  x
assocL  (Right (Left  x)) = Left $ Right x
assocL ~(Right (Right x)) =        Right x

-- | See `assocL`
assocR :: Either (Either a b) c -> Either a (Either b c)
assocR  (       Right x)  = Right $ Right x
assocR  (Left  (Right x)) = Right $ Left  x
assocR ~(Left  (Left  x)) =         Left  x

-- | Map a function to all `Left`
mapLeft  :: (a -> b) -> Either a c -> Either b c
mapLeft f  (Left  x) = Left  $ f x
mapLeft _ ~(Right x) = Right     x

-- | See `mapLeft` (implemented as fmap)
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight = fmap

