{-# LANGUAGE OverloadedStrings #-}

module WebUI where

import           EqCommon
import           EqLessons
import           EqSQL
import           System.Exit       (exitWith, ExitCode(ExitFailure, ExitSuccess))
import qualified Web.Scotty     as Web
import           Data.Monoid       (mconcat)
import qualified Data.Text      as Text
import           Data.Text         (Text)
import qualified Data.Text.Lazy as Lazy
import           Data.Foldable     (toList)
import           Data.List         (intercalate)

tbLesson :: Lesson -> Text
tbLesson l = Text.pack $ "<td>" ++ intercalate "</td><td>" [c,s,o,n,a,r] ++ "</td>"
       where c = show $ chapter l
             s = [section l]
             o = show $ count l
             n = Text.unpack $ lName l
             a = if (score l) == (-1) then "" else show $ score l
             r = show $ adaptedScore l

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do Web.get "/"
                  $ do let a   = blankAssessment Eq2 "1467" "Example"
                           t   = Lazy.fromStrict $ teacher a
                           s   = Lazy.fromStrict $ student a
                           ls  = toList $ (Lazy.fromStrict . tbLesson) <$> (lessons a)
                           rs  = zip3 (repeat "<tr><td>") ls $ repeat "</td></tr>"
                           trs = fn <$> rs
                           fn (a,b,c) = Lazy.append (Lazy.append a b) c
                       Web.html $ mconcat [ "<!DOCTYPE html><html>"
                                          , "<h1>Blank Assessment:</h1>"
                                          , "<p>",t,"</p>"
                                          , "<p>",s,"</p>"
                                          , "<table>"
                                          , mconcat trs
                                          , "</table>"
                                          , "</html>"
                                          ]
