{-|
Module      : Data.Git.Details.Parse
Description : Parse data types for getting the details of git projects
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module Data.Git.Details.Parse where


-- import Control.Monad
import Data.Attoparsec.Text ( Parser
                            , endOfInput
                            , manyTill
                            , skipSpace
                            )
import Data.Attoparsec.Text.Utils (skipChar, skipN, skipLine, takeLine)
import qualified Data.Map.Strict as Map
import Git.Types.Parse (parseLogLine)
import Data.Git.Details (Details(..))


-- | Parser for `Details`
-- | Examples parsed:
--
-- @
-- * remote origin
--   Fetch URL: https://github.com/michaeljklein/prim-spoon.git
--   Push  URL: https://github.com/michaeljklein/prim-spoon.git
--   HEAD branch: master
--   Remote branch:
--     master tracked
--   Local branch configured for 'git pull':
--     master merges with remote master
--   Local ref configured for 'git push':
--     master pushes to master (up to date)
-- @
-- @
-- * remote origin
--   Fetch URL: https://github.com/michaeljklein/CPlug.git
--   Push  URL: https://github.com/michaeljklein/CPlug.git
--   HEAD branch: master
--   Remote branch:
--     master tracked
--   Local branch configured for 'git pull':
--     master merges with remote master
--   Local ref configured for 'git push':
--     master pushes to master (fast-forwardable)
-- @
--
detailsParser :: Parser Details
detailsParser = do
  skipLine                            -- Skip "* remote origin"
  skipN 13                            -- Skip "  Fetch URL: "
  fetchURL'   <- takeLine
  skipN 13                            -- Skip "  Push  URL: "
  pushURL'    <- takeLine
  skipN 15                            -- Skip "  HEAD branch: "
  headBranch' <- takeLine
  skipLine                            -- Skip "  Remote branch:"
  skipSpace
  remoteBranch' <- takeLine
  skipLine                            -- Skip "  Local branch configured for 'git pull':"
  skipSpace
  localPullBranch' <- takeLine
  skipLine                            -- Skip "  Local branch configured for 'git pull':"
  skipSpace
  localPushBranch' <- takeLine
  _ <- skipChar `manyTill` endOfInput
  return $ Details { _fetchURL=fetchURL'
              , _pushURL=pushURL'
              , _headBranch=headBranch'
              , _remoteBranch=remoteBranch'
              , _localPullBranch=localPullBranch'
              , _localPushBranch=localPushBranch'
              }

