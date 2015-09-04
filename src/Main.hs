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
      , "  -h, --help         Print this help and exit"
      , "  -v, --version      Print the version and exit"
      , "  -d, --dbinit [N]   Initialize a db named N for eqassess (“EqDB” by default)"
      , "  -w, --web [P]      Start the web interface on port P (8080 by default)"
      ]

main :: IO ()
main = getArgs >>= dispatch . parseArgs

exitFail, exitSucc :: IO ()
exitFail = exitWith $ ExitFailure 1
exitSucc = exitWith   ExitSuccess

data EqAssessConf = Config { eqHelp    :: Bool
                           , eqVersion :: Bool
                           , eqInitDB  :: Bool
                           , eqDbName  :: String
                           , eqPortNum :: Int
                           }

dispatch :: EqAssessConf -> IO ()
dispatch (Config h v i d p) | h         = putStrLn usage   >> exitSucc
                            | v         = putStrLn version >> exitSucc
                            | i         = mapM_ (initDB d) [Eq2]
                            | p > 0     = runWebServer p
                            | otherwise = putStrLn usage   >> exitFail

parseArgs :: [String] -> EqAssessConf
parseArgs as = Config { eqHelp    = as `hasArg` ("-h", "--help"   )
                      , eqVersion = as `hasArg` ("-v", "--version")
                      , eqInitDB  = as `hasArg` ("-d", "--dbinit" )
                      , eqDbName  = optArg ("-d", "--dbinit","EqDB") as
                      , eqPortNum = (read $ optArg ("-w", "--web", "8080") as) :: Int
                      } where hasArg a (s,l)    = elem s a || elem l a
                              nextArg s l       = (drop (1 + (fromJust $ elemIndex s l)) l) !! 0
                              optArg (s,l,d) ls | elem s ls = nextArg s ls
                                                | elem l ls = nextArg l ls
                                                | otherwise = d
