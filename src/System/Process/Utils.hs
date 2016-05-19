{-|
Module      : System.Process.Utils
Description : Convenience functions for calling commands
Copyright   : (c) Michael Klein, 2016
License     : BSD3
Maintainer  : lambdamichael(at)gmail.com
-}

module System.Process.Utils where


import System.Process ( readProcessWithExitCode
                      )
import System.Exit    ( ExitCode(..)
                      )


-- | A `String` containing errors
type ErrString = String

-- | Do `readProcessWithExitCode`, returning @`Left` errorDetails@ on failure
simpleRun :: String -> [String] -> String -> IO (Either ErrString String)
simpleRun cmd args input = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args input
  if (exitCode /= ExitSuccess) || (stdErr /= "")
  then do
    return . Left  $ unlines [ "exitCode:"
                        , show exitCode
                        , "stderr:"
                        , stdErr
                        ]
  else do
    return . Right $ stdOut

