module Main where

import EqCommon
import EqLessons
import EqSQL
import WebUI
import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Data.List
import Data.Maybe

version :: String
version = "eqassess 2.1"

usage :: String
usage = intercalate "\n"
      [ "Usage: eqassess [options]\n"
      , "Options:"
      , "  -h, --help      Print this help and exit"
      , "  -v, --version   Print the version and exit"
      , "  -w, --web[=P]   Start the web interface on port P (8080 by default)"
      ]

main :: IO ()
main = getArgs >>= parse

exitFail, exitSucc :: IO ()
exitFail = exitWith $ ExitFailure 1
exitSucc = exitWith   ExitSuccess

parse :: [String] -> IO ()
parse a | a `hasArg` ("-h","--help"   ) = putStrLn usage   >> exitSucc
        | a `hasArg` ("-v","--version") = putStrLn version >> exitSucc
        | a `hasArg` ("-w","--web"    ) = runWebServer portNum
        | otherwise                     = putStrLn usage   >> exitFail
        where hasArg as (s,l)   = s `elem` as || l `elem` as
              nextIdx o as      = (fromJust $ elemIndex o as) + 1
              argOpt (s,l,d) as | elem s as && isValidIdx (n s) as = (read $ as !! n s) :: Int
                                | elem l as && isValidIdx (n l) as = (read $ as !! n l) :: Int
                                | otherwise                        = d
                                where n o = nextIdx o as
              isValidIdx n as   = n >= 0 && n < length as
              portNum           = argOpt ("-w","--web",8080) a
