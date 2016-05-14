module System.Process.Utils where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- | Do `readProcessWithExitCode`, returning @`Left` errorDetails@ on failure
simpleRun :: String -> [String] -> String -> IO (Either String String)
simpleRun cmd args input = do
  (exitCode, stdOut, stdErr) <- readProcessWithExitCode cmd args input
  if (exitCode /= ExitSuccess) || (stdErr /= "")
  then do
    return . Left  $ unlines ["exitCode:", show exitCode, "stderr:", stdErr]
  else do
    return . Right $ stdOut
