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
import           Data.List         (intercalate, nub, foldl')
import           Control.Monad.IO.Class    (liftIO)

tbLesson :: Lesson -> Text
tbLesson l = Text.pack $ concat ["<tr class=\"",c,"\">","<td>",s,"</td>"
                                ,"<td style=\"text-align: center;\">",a,"</td>"
                                ,"<td>",n,"</td>","</tr>"]
       where n = Text.unpack $ lName l
             c = Text.unpack . head . toList $ tags l
             r = score l
             i = show (chapter l, section l, count l)
             ch = [if r == x then " checked" else "" | x <- [(-1)..1]]
             t = "<input type=\""
             m = concat ["\" name=\"",show (chapter l, section l, count l)]
             s = concat [t,"radio",m,"\" value=\"(Just (-1),Nothing)\"",ch !! 0,"> blank"
                        ,t,"radio",m,"\" value=\"(Just 0,Nothing)\"", ch !! 1,"> 0"
                        ,t,"radio",m,"\" value=\"(Just 1,Nothing)\"", ch !! 2,"> 1"]
             a = concat [t,"checkbox",m,"\" value=\"(Nothing,Just True)\"",b,">"]
             b = if adapted l then " checked" else ""

headers :: Lazy.Text
headers = mconcat [ "<!DOCTYPE html><html><head>"
                  , "<meta charset=\"UTF-8\">"
                  , css
                  , "<title>Equals Assessment</title>"
                  , "<link rel='icon' href='",ico,"' type='image/x-icon' />"
                  , "<link rel='shortcut icon' href='",ico,"' type='image/x-icon' />"
                  , "</head>"
                  , "<body><img src=\"https://www.ablenetinc.com/Portals/0/images/Equals-Online-Assessment-LogIn.jpg\">"
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
                                                         , "}"
                                                         , mconcat ["window.onload = function () { showRows('",head (nub tgs),"');"]
                                                         , "};</script>"
                                                         ]
                          Web.html $ mconcat [ headers
                                             , js
                                             , "<form method=\"POST\" action=\"/save\" enctype=\"multipart/form-data\">"
                                             , "<p>Assessment by ",t," for ",s,": "
                                             , "<input type=\"submit\" name=\"s\" value=\"Export\"> "
                                             , "<input type=\"submit\" name=\"s\" value=\"Save\"></p><br>"
                                             , tbs
                                             , "<br><table>"
                                             , "<tr id=\"heading\"><th>Score</th><th>Adapted</th><th>Test Name</th></tr>"
                                             , mconcat ls
                                             , "</table>"
                                             , "<input type=\"radio\" name=\"v\" value=\"Eq2\" style=\"visibility: hidden;\" checked><br>"
                                             , "<input type=\"text\" name=\"u\" value=\"",t,"\" style=\"visibility: hidden;\"><br>"
                                             , "<input type=\"text\" name=\"i\" value=\"",s,"\" style=\"visibility: hidden;\"><br>"
                                             , "</form></body></html>"
                                             ]

                  Web.post "/save" $ do
                           p       <- Web.params
                           ret     <- Web.param "s"
                           version <- Web.param "v"
                           teacher <- Web.param "u"
                           student <- Web.param "i"
                           let v   = (read version) :: EqVersion
                               t   = teacher :: String
                               s   = student :: String
                               as  = blankAssessment v student teacher
                               scs = drop 1 $ take (length p - 3) p
                               nls = (read . Lazy.unpack . fst <$> scs) :: [(Int,Char,Int)]
                               nss = (read . Lazy.unpack . snd <$> scs) :: [(Maybe Int,Maybe Bool)]
                               nas = foldl' (\x (y,z) -> updateLesson x y z) as $ zip nls nss
                           na      <- liftIO $ saveAssessment "EqDB" nas
                           case (ret :: String) of
                                "Export" -> Web.text . Lazy.fromStrict $ toCSV nas
                                "Save"   -> Web.redirect . Lazy.pack $ concat ["/assess?u=",teacher,"&i=",student,"&v=",version,"&c=Load"]
