module Main where

import EqCommon
import EqLessons
import EqSQL
import WebUI
import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Data.List

version :: String
version = "eqassess 2.1"

usage :: String
usage = intercalate "\n"
      [ "Usage: eqassess [options]\n"
      , "Options:"
      , "  -h, --help      Print this help and exit"
      , "  -v, --version   Print the version and exit"
      , "  -w, --web=PNUM  Start the web interface on port PNUM"
      ]

main :: IO ()
main = getArgs >>= parse

exitFail, exitSucc :: IO ()
exitFail = exitWith $ ExitFailure 1
exitSucc = exitWith   ExitSuccess

parse :: [String] -> IO ()
parse a | elem "-h" a || elem "--help"    a = putStrLn usage   >> exitSucc
        | elem "-v" a || elem "--version" a = putStrLn version >> exitSucc
        | elem "-w" a || elem "--web"     a = runWebServer
        | otherwise                         = putStrLn usage   >> exitFail
