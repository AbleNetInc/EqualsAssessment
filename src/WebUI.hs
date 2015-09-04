module WebUI where

import EqCommon
import EqLessons
import EqSQL
import System.Exit        (exitWith, ExitCode(ExitFailure, ExitSuccess))

runWebServer :: Int -> IO ()
runWebServer pnum = putStrLn $ show pnum

