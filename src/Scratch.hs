module Scratch where

import Data.Conduit.Process.Utils
import Conduit
import Data.Conduit.Attoparsec
import Data.Git.Details
import Data.Git.Details.Parse
import Data.ByteString (ByteString)

-- git (info?)
-- detailsProducer :: MonadIO m => Producer m DetailsS
-- detailsProducer

a = singleProcS "git" ["remote", "show", "origin"] :: Source IO ProcessSource

b = decodeUtf8LenientC =$= conduitParserEither detailsParser =$= mapC (fmap snd) :: Conduit ByteString IO (Either ParseError Details)
