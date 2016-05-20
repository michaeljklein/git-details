{-|
Module      : Data.Git.Details
Description : Data types for getting the details of git projects
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Details where


import Data.Text (Text)


-- | Details of a @git@ project
data Details = Details { _fetchURL         :: Text
                       , _pushURL          :: Text
                       , _headBranch       :: Text
                       , _remoteBranch     :: Text
                       , _localPullBranch  :: Text
                       , _localPushBranch  :: Text
                       } deriving (Eq)

-- | Empty details (all fields are empty)
emptyDetails :: Details
emptyDetails = Details { _fetchURL=""
                       , _pushURL=""
                       , _headBranch=""
                       , _remoteBranch=""
                       , _localPullBranch=""
                       , _localPushBranch=""
                       }

