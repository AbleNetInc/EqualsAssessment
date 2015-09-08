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
tbLesson l = Text.pack $ "<td>" ++ intercalate "</td><td>" [n,s,a] ++ "</td>"
       where s = "<input type=\"radio\" name=\"" ++ n ++ "_score\" value=\"-1\" checked> blank"
              ++ "<input type=\"radio\" name=\"" ++ n ++ "_score\" value=\"0\"> 0"
              ++ "<input type=\"radio\" name=\"" ++ n ++ "_score\" value=\"1\"> 1"
             n = Text.unpack $ lName l
             a = "<input type=\"checkbox\" name\"" ++ n ++ "_adapted\" value=\"adapted\">"

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do Web.get "/:teacher/:student"
                  $ do teacher <- Web.param "teacher"
                       student <- Web.param "student"
                       let a   = blankAssessment Eq2 student teacher
                           t   = Lazy.pack teacher
                           s   = Lazy.pack student
                           ls  = toList $ (Lazy.fromStrict . tbLesson) <$> (lessons a)
                           rs  = zip3 (repeat "<tr>") ls $ repeat "</tr>"
                           trs = fn <$> rs
                           fn (a,b,c) = Lazy.append (Lazy.append a b) c
                           ico = "https://www.ablenetinc.com/media/favicon/default/favicon.ico"
                       Web.html $ mconcat [ "<!DOCTYPE html><html><head>"
                                          , "<meta charset=\"UTF-8\">"
                                          , "<title>Equals Assessment</title>"
                                          , "<link rel='icon' href='",ico,"' type='image/x-icon' />"
                                          , "<link rel='shortcut icon' href='",ico,"' type='image/x-icon' />"
                                          , "</head><body><h1>Assessment by ",t," for ",s,":</h1>"
                                          , "<form method=\"post\" enctype=\"multipart/form-data\">"
                                          , "<table>"
                                          , mconcat trs
                                          , "</table></form></body></html>"
                                          ]
