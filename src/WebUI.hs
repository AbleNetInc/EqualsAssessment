{-# LANGUAGE OverloadedStrings #-}

module WebUI where

import           EqCommon
import           EqLessons
import           EqSQL
import qualified Web.Scotty                            as Web
import           Data.Monoid                              (mconcat)
import qualified Data.Text                             as Text
import           Data.Text                                (Text)
import qualified Data.Text.Lazy                        as Lazy
import           Data.Foldable                            (toList)
import           Data.List                                (intercalate, nub, foldl')
import           Control.Monad.IO.Class                   (liftIO)
import           Network.Wai.Middleware.RequestLogger     (logStdoutDev)

tbLesson :: Lesson -> Text
tbLesson l = Text.pack $ concat ["<tr class=\"",c,"\">","<td>",s,"</td>"
                                ,"<td style=\"text-align: center;\">",a,"</td>"
                                ,"<td>",n,"</td>","</tr>"]
       where n = if m == "(1,'C',4)" then adj else Text.unpack $ lName l
             adj = "identify primary and secondary colors"
             c = if hidden then "hidden" else Text.unpack . head . toList $ tags l
             r = score l
             m = show (chapter l, section l, count l)
             o = concat ["(",show $ chapter l,",\\'",[section l],"\\',",show $ count l + 1,")"]
             ch = [if r == x then " checked" else "" | x <- [(-1)..1]]
             t = "<input type=\""
             oc = [ if m == "(1,'C',4)" || m == "(9,'A',3)" then concat ["onchange=\"copyScore('",o,"','",val,"');\" "] else ""
                  | val <- ["(Just (-1),Nothing)","(Just 0,Nothing)","(Just 1,Nothing)"]]
             cc = if m == "(1,'C',4)" || m == "(9,'A',3)" then concat ["onchange=\"copyAdapt(this,'",o,"');\" "] else ""
             s = concat [t,"radio\" name=\"",m,"\" ",oc !! 0,"value=\"(Just (-1),Nothing)\"",ch !! 0,"> blank"
                        ,t,"radio\" name=\"",m,"\" ",oc !! 1,"value=\"(Just 0,Nothing)\"", ch !! 1,"> 0"
                        ,t,"radio\" name=\"",m,"\" ",oc !! 2,"value=\"(Just 1,Nothing)\"", ch !! 2,"> 1"]
             a = concat [t,"checkbox\" name=\"",m,"\" ",cc,"value=\"(Nothing,Just True)\"",b,">"]
             b = if adapted l then " checked" else ""
             hidden = m == "(1,'C',5)" || m == "(9,'A',4)"

headers :: Lazy.Text
headers = mconcat [ "<!DOCTYPE html><html><head>"
                  , "<meta charset=\"UTF-8\">"
                  , css
                  , "<title>Equals Assessment</title>"
                  , "<link rel='icon' href='",hme,ico,"' type='image/x-icon' />"
                  , "<link rel='shortcut icon' href='",hme,ico,"' type='image/x-icon' />"
                  , "</head>"
                  , "<body><div style=\"width: 770px; margin: auto;\">"
                  , "<img style=\"margin-left: 11.5%;\" src=\"",hme,img,"\">"
                  , "<p style=\"color: red;\"> Note:"
                  , "<span style=\"font-style: italic;\">All</span> "
                  , "Assessment Data is deleted each night at midnight (Central Time)</p>"
                  ] where hme = "https://www.ablenetinc.com/"
                          ico = "media/favicon/default/favicon.ico"
                          img = "Portals/0/images/Equals-Online-Assessment-LogIn.jpg"
                          css = Lazy.intercalate " " [ "<style>"
                                                     , "body {"
                                                     ,    "margin: 0;"
                                                     ,    "padding: 0;"
                                                     , "}"
                                                     , "table, th, td {"
                                                     ,    "border: 1px solid black;"
                                                     ,    "border-collapse: collapse;"
                                                     ,    "padding: 5px;"
                                                     , "}"
                                                     , ".tab {"
                                                     ,    "background: #eee;"
                                                     ,    "padding: 2px;"
                                                     ,    "border: 1px solid #bbb;"
                                                     ,    "text-decoration: none;"
                                                     ,    "font-size: 13px;"
                                                     , "}"
                                                     , ".selected {"
                                                     ,    "background: #fff;"
                                                     ,    "padding: 2px;"
                                                     ,    "border: 1px solid #bbb;"
                                                     ,    "text-decoration: none;"
                                                     ,    "font-size: 13px;"
                                                     , "}"
                                                     , ".hidden {"
                                                     ,    "display: none;"
                                                     , "}"
                                                     , "</style>"
                                                     ]

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do
                  Web.middleware logStdoutDev

                  Web.get "/" $ do
                          Web.html $ mconcat [ headers
                                             , "<form method=\"GET\" action=\"/assess\">"
                                             , "Username: <input type=\"text\" name=\"u\"><br>"
                                             , "Student ID: <input type=\"number\" min=\"0\" name=\"i\"><br>"
                                             , "<input class=\"hidden\" type=\"radio\" name=\"v\" value=\"Eq2\" checked><br>"
                                             , "<input type=\"submit\" name=\"c\" value=\"Load\">"
                                             , "<input type=\"submit\" name=\"c\" value=\"New\">"
                                             , "</div></body></html>"
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
                              nav n = mconcat ["<a class=\"tab\" id=\"",n,"\" href=\"#\" onclick=\"showRows('",n,"')\">",n,"</a>"]
                              tbs = mconcat ["<nav>", (mconcat $ nav <$> (nub tgs)), "</nav>"]
                              js  = Lazy.intercalate " " [ "<script>"
                                                         , "function showRows(id) {"
                                                         ,   "var trs = document.getElementsByTagName(\"tr\");"
                                                         ,   "for (i = 0; i < trs.length; i++) {"
                                                         ,     "trs[i].style.display = \"none\";"
                                                         ,   "}"
                                                         ,   "var ls = document.getElementsByClassName(id);"
                                                         ,   "for (i = 0; i < ls.length; i++) {"
                                                         ,     "ls[i].style.display = \"table-row\";"
                                                         ,   "}"
                                                         ,   "document.getElementById(\"heading\").style.display = \"table-row\";"
                                                         ,   "var as = document.getElementsByTagName(\"a\");"
                                                         ,   "for (i = 0; i < as.length; i++) {"
                                                         ,     "as[i].className = \"tab\";"
                                                         ,   "}"
                                                         ,   "document.getElementById(id).className = \"selected\";"
                                                         , "}"
                                                         , "function copyScore(id,val) {"
                                                         ,   "var exs = document.getElementsByName(id);"
                                                         ,   "for (i = 0; i < exs.length; i++) {"
                                                         ,     "exs[i].value = val;"
                                                         ,   "}"
                                                         , "}"
                                                         , "function copyAdapt(self,id) {"
                                                         ,   "var exs = document.getElementsByName(id);"
                                                         ,   "for (i = 0; i < exs.length; i++) {"
                                                         ,     "if (exs[i].type != \"checkbox\") { continue; };"
                                                         ,     "exs[i].value = self.checked == true ? \"(Nothing,Just True)\" : \"(Nothing,Just False)\";"
                                                         ,   "}"
                                                         , "}"
                                                         , mconcat ["window.onload = function () { showRows('",head (nub tgs),"');"]
                                                         , "};"
                                                         , "</script>"
                                                         ]
                          Web.html $ mconcat [ headers
                                             , js
                                             , "<form method=\"POST\" action=\"/save.csv\" enctype=\"multipart/form-data\">"
                                             , "<p>", if a == as && (score <$> ll) == (score <$> (lessons as)) then "New " else ""
                                             , "Assessment by ",t," for ",s,": <p>"
                                             , "<input type=\"submit\" name=\"s\" value=\"Export\"> "
                                             , "<input type=\"submit\" name=\"s\" value=\"Save\"><br>"
                                             , tbs
                                             , "<table style=\"margin-top: 1px; width: 770px;\">"
                                             , "<tr id=\"heading\"><th>Score</th><th>Adapted</th><th>Test Name</th></tr>"
                                             , mconcat ls
                                             , "</table>"
                                             , "<input class=\"hidden\" type=\"radio\" name=\"v\" value=\"Eq2\" checked><br>"
                                             , "<input class=\"hidden\" type=\"text\" name=\"u\" value=\"",t,"\"><br>"
                                             , "<input class=\"hidden\" type=\"text\" name=\"i\" value=\"",s,"\"><br>"
                                             , "</form></div></body></html>"
                                             ]

                  Web.post "/save.csv" $ do
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
                                "Export" -> do sf <- liftIO $ saveFile nas
                                               Web.setHeader (Lazy.pack "Content-Type") (Lazy.pack "text/csv")
                                               Web.file $ t ++ "_" ++ s ++ ".csv"
                                "Save"   -> Web.redirect . Lazy.pack $ concat ["/assess?u=",teacher,"&i=",student,"&v=",version,"&c=Load"]
