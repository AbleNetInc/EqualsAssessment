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
tbLesson l = Text.pack $ "<td>" ++ intercalate "</td><td>" [s,a,n] ++ "</td>"
       where n = Text.unpack $ lName l
             i = filter (`notElem` ['\"', ' ']) n
             s = "<input type=\"radio\" name=\"" ++ i ++ "_score\" value=\"-1\" checked> blank"
              ++ "<input type=\"radio\" name=\"" ++ i ++ "_score\" value=\"0\"> 0"
              ++ "<input type=\"radio\" name=\"" ++ i ++ "_score\" value=\"1\"> 1"
             a = "<input type=\"checkbox\" name=\"" ++ i ++ "_adapted\" value=\"adapted\">"

headers :: Lazy.Text
headers = mconcat [ "<!DOCTYPE html><html><head>"
                  , "<meta charset=\"UTF-8\">"
                  , css
                  , "<title>Equals Assessment</title>"
                  , "<link rel='icon' href='",ico,"' type='image/x-icon' />"
                  , "<link rel='shortcut icon' href='",ico,"' type='image/x-icon' />"
                  , "</head>"
                  ] where ico = "https://www.ablenetinc.com/media/favicon/default/favicon.ico"
                          css = Lazy.intercalate "\n" [ "<style>"
                                                      , "table, th, td {"
                                                      ,    "border: 1px solid black;"
                                                      ,    "border-collapse: collapse;"
                                                      ,    "padding: 5px;"
                                                      , "}"
                                                      , "</style>"
                                                      ]

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum
                  $ do Web.get "/:teacher/:student"
                       $ do teacher <- Web.param "teacher"
                            student <- Web.param "student"
                            let a   = blankAssessment Eq2 student teacher
                                t   = Lazy.pack teacher
                                s   = Lazy.pack student
                                ls  = toList $ (Lazy.fromStrict . tbLesson) <$> (lessons a)
                                rs  = zip3 (repeat "<tr>") ls $ repeat "</tr>"
                                trs = fn <$> rs
                                fn (a,b,c) = Lazy.append (Lazy.append a b) c
                            Web.html $ mconcat [ headers
                                               , "<body><h1>Assessment by ",t," for ",s,":</h1>"
                                               , "<form method=\"post\" enctype=\"multipart/form-data\">"
                                               , "<table>"
                                               , "<tr><th>Score</th><th>Adapted</th><th>Test Name</th></tr>"
                                               , mconcat trs
                                               , "</table></form></body></html>"
                                               ]
