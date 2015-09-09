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
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.Foldable     (toList)
import           Data.List         (intercalate, nub)
import           Control.Monad.IO.Class    (liftIO)

tbLesson :: Lesson -> Text
tbLesson l = Text.pack $ concat ["<tr class=\"",c,"\" style=\"display: none;\">","<td>",s,"</td><td style=\"text-align: center;\">",a,"</td><td>",n,"</td>","</tr>"]
       where n = Text.unpack $ lName l
             c = Text.unpack . head . toList $ tags l
             r = score l
             i = filter (`notElem` ['\"', ' ']) n
             ch = [y | x <- [(-1)..1], let y = if r == x then "checked" else ""]
             s = concat ["<input type=\"radio\" name=\"",i,"_score\" value=\"-1\" ",ch !! 0,"> blank"
                        ,"<input type=\"radio\" name=\"",i,"_score\" value=\"0\" ",ch !! 1,"> 0"
                        ,"<input type=\"radio\" name=\"",i,"_score\" value=\"1\" ",ch !! 2,"> 1"]
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
                          css = Lazy.intercalate " " [ "<style>"
                                                     , "table, th, td {"
                                                     ,    "border: 1px solid black;"
                                                     ,    "border-collapse: collapse;"
                                                     ,    "padding: 5px;"
                                                     , "}"
                                                     , "</style>"
                                                     ]

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do
                  Web.get "/" $ do
                          Web.html $ mconcat [ headers
                                             , "<body><h1>Load or Start an Assessment:</h1>"
                                             , "<form method=\"GET\" action=\"/assess\">"
                                             , "Username: <input type=\"text\" name=\"u\"><br>"
                                             , "Student ID: <input type=\"text\" name=\"i\"><br>"
                                             , "<input type=\"radio\" name=\"v\" value=\"Eq2\" style=\"visibility: hidden;\" checked><br>"
                                             , "<input type=\"submit\" name=\"c\" value=\"Load\">"
                                             , "<input type=\"submit\" name=\"c\" value=\"New\">"
                                             , "</body></html>"
                                             ]

                  Web.get "/assess" $ do
                          teacher <- Web.param "u"
                          student <- Web.param "i"
                          version <- Web.param "v"
                          clobber <- Web.param "c"
                          let v   = (read version) :: EqVersion
                          a       <- liftIO $ retrieveAssessment "EqDB" v student teacher
                          let as  = blankAssessment v student teacher
                              t   = Lazy.pack teacher
                              s   = Lazy.pack student
                              ll  = lessons $ case (clobber :: String) of
                                                   "New"  -> as
                                                   "Load" -> a
                              ls  = toList $ (Lazy.fromStrict . tbLesson) <$> ll
                              tgs = Lazy.fromStrict <$> (concat $ (toList . tags) <$> ll)
                              nav n = mconcat [" <a href=\"#\" onclick=\"showRows('",n,"')\">",n,"</a> |"]
                              tbs = mconcat ["<nav>| ", (mconcat $ nav <$> (nub tgs)), "</nav>"]
                              js  = Lazy.intercalate " " [ "<script>function showRows(id) {"
                                                         ,   "var trs = document.getElementsByTagName(\"tr\");"
                                                         ,   "for (i = 0; i < trs.length; i++) {"
                                                         ,     "trs[i].style.display = \"none\";"
                                                         ,   "}"
                                                         ,   "var ls = document.getElementsByClassName(id);"
                                                         ,   "for (i = 0; i < ls.length; i++) {"
                                                         ,     "ls[i].style.display = \"table-row\";"
                                                         ,   "}"
                                                         ,   "document.getElementById(\"heading\").style.display = \"table-row\";"
                                                         , "}" , mconcat ["window.onload = function () { showRows('",head (nub tgs),"'); };</script>"]
                                                         ]
                          Web.html $ mconcat [ headers
                                             , "<body><h1>Assessment by ",t," for ",s,":</h1>"
                                             , js
                                             , "<form method=\"POST\" action=\"/save\" enctype=\"multipart/form-data\">"
                                             , "<input type=\"submit\" name=\"s\" value=\"Export\">"
                                             , "<input type=\"submit\" name=\"s\" value=\"Save\"><br><br>"
                                             , tbs
                                             , "<br><table>"
                                             , "<tr id=\"heading\"><th>Score</th><th>Adapted</th><th>Test Name</th></tr>"
                                             , mconcat ls
                                             , "</table></form></body></html>"
                                             ]

                  Web.post "/save" $ do
                           ret <- Web.param "s"
                           b   <- Web.body
                           Web.text $ case (ret :: String) of
                                "Export" -> Lazy.fromStrict . toCSV $ blankAssessment Eq2 "test" "test"
                                "Save"   -> decodeUtf8 b
