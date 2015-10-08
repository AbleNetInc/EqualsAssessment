module Main where

import EqCommon
import EqSQL
import WebUI
import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(ExitFailure, ExitSuccess))
import Data.List          (intercalate, elemIndex)
import Data.Maybe         (fromJust)

version :: String
version = "eqassess 2.1"

usage :: String
usage = intercalate "\n"
      [ "Usage: eqassess [options]\n"
      , "Options:"
      , "  -h, --help       Print this help and exit"
      , "  -v, --version    Print the version and exit"
      , "  -d, --dbinit N   Initialize a db named N for eqassess"
      , "  -w, --web P      Start the web interface on port P"
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
                           , eqRunServ :: Bool
                           , eqPortNum :: Int
                           }

dispatch :: EqAssessConf -> IO ()
dispatch (Config h v i d r p) | h         = putStrLn usage   >> exitSucc
                              | v         = putStrLn version >> exitSucc
                              | r         = runWebServer p
                              | i         = mapM_ (initDB d) [Eq2]
                              | otherwise = putStrLn usage   >> exitFail

parseArgs :: [String] -> EqAssessConf
parseArgs as = Config { eqHelp    = as `hasArg` ("-h", "--help"   )
                      , eqVersion = as `hasArg` ("-v", "--version")
                      , eqInitDB  = as `hasArg` ("-d", "--dbinit" )
                      , eqRunServ = as `hasArg` ("-w", "--web"    )
                      , eqDbName  = optArg ("-d", "--dbinit") as
                      , eqPortNum = (read $ optArg ("-w", "--web") as) :: Int
                      } where hasArg a (s,l)  = elem s a || elem l a
                              nextArg s l     = l !! (1 + (fromJust $ elemIndex s l))
                              optArg (s,l) ls | elem s ls = nextArg s ls
                                              | elem l ls = nextArg l ls
                              optArg (_,_) _  = ""
