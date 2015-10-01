{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module WebUI where

import           EqCommon
import           EqLessons
import           EqSQL
import qualified Web.Scotty                            as Web
import           Lucid.Base
import           Lucid.Html5
import           Data.Monoid                              (mconcat)
import qualified Data.Text                             as Text
import           Data.Text                                (Text)
import qualified Data.Text.Lazy                        as Lazy
import           Data.Foldable                            (toList)
import           Data.List                                (intercalate, nub, foldl')
import           Control.Monad.IO.Class                   (liftIO)
import           Network.Wai.Middleware.RequestLogger     (logStdoutDev)

tbLesson :: Lesson -> Html ()
tbLesson l = tr_ [class_ c] $ do td_ ""; td_ s; td_ [style_ "text-align: center;"] a; td_ n
       where n = if m == "(1,'C',4)" then adj else toHtml . fst $ lName l
             adj = "identify primary and secondary colors"
             c = if hidden then "hidden" else head . toList $ tags l
             r = score l
             m = Text.pack $ show (chapter l, section l, count l)
             o = Text.pack $ concat ["(",show $ chapter l,",\\'",[section l],"\\',",show $ count l + thing,")"]
             thing | m == "(9,'A',5)" = (-1)
                   | otherwise        = 1
             ch = [if r == x then checked_ else alt_ "" | x <- [(-1)..1]]
             oc = [if m == "(1,'C',4)" || m == "(9,'A',5)" then onchange_ (mconcat ["copyScore('",o,"','",val,"')"]) else alt_ ""
                  | val <- ["(Just (-1),Nothing)","(Just 0,Nothing)","(Just 1,Nothing)"]]
             cc = if m == "(1,'C',4)" || m == "(9,'A',5)" then onchange_ (mconcat ["copyAdapt(this,'",o,"')"]) else alt_ ""
             s = do input_ [type_ "radio", name_ m, oc !! 0, value_ "(Just (-1),Nothing)", ch !! 0]; "blank"
                    input_ [type_ "radio", name_ m, oc !! 1, value_ "(Just 0,Nothing)",    ch !! 1]; "0"
                    input_ [type_ "radio", name_ m, oc !! 2, value_ "(Just 1,Nothing)",    ch !! 2]; "1"
             a = input_ [type_ "checkbox", name_ m, cc, value_ "(Nothing,Just True)",  b]
             b = if adapted l then checked_ else alt_ ""
             hidden = m == "(1,'C',5)" || m == "(9,'A',4)"

css :: Text
css = Text.intercalate " "
    [ "body {"
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
    ]

home, ico, bnr :: Text
home = "https://www.ablenetinc.com/"
ico  = mconcat [home, "media/favicon/default/favicon.ico"]
bnr  = mconcat [home, "Portals/0/images/Equals-Online-Assessment-LogIn.jpg"]

header :: Html ()
header = head_ $ do
     meta_ [charset_ "UTF-8"]
     style_ css
     title_ "Equals Assessment"
     link_ [rel_ "icon", href_ ico, type_ "image/x-icon"]
     link_ [rel_ "shortcut icon", href_ ico, type_ "image/x-icon"]

banner :: Html ()
banner = do img_ [style_ "margin-left: 11.5%", src_ bnr]
            p_ [style_ "color: red;"] $
               mconcat ["Note: While you may save your data, ",all," assessment data is deleted each night at midnight (Central Time)"]
            where all = span_ [style_ "font-style: italic;"] "all"

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do
                  Web.middleware logStdoutDev

                  Web.get "/" $ do
                          Web.html . renderText $ do doctypehtml_
                                 $ do header
                                      body_ $ div_ [style_ "width: 770px; margin: auto;"] $ do
                                          let italic = span_ [style_ "font-style: italic;"]
                                          banner
                                          p_ "To get started, complete the assessment and fill out the paper copy of the student response booklet (protocol). Then:"
                                          ol_ $ do li_ $ mconcat ["Fill out the ",italic "Username"," field with your name."]
                                                   li_ $ mconcat ["Enter a ",italic "Student ID"," number in case you need to leave and return to finish scoring later in the day. Only numbers are valid."]
                                                   li_ $ mconcat ["Click ",italic "New"," to enter data for the student."]
                                          p_ $ mconcat ["To retrieve data saved earlier in the day, enter the ",italic "Username"," and ",italic "Student ID"," and click ",italic "Load","."]
                                          with form_ [method_ "GET", action_ "/assess"] $ do
                                             p_ $ do "Username: "; input_ [type_ "text", name_ "u", required_ ""]
                                                     br_ []
                                                     "Student ID: "; input_ [type_ "number", min_ "0", name_ "i", required_ ""]
                                             input_ [class_ "hidden", type_ "radio", name_ "v", value_ "Eq2", checked_]
                                             input_ [type_ "submit", name_ "c", value_ "Load"]
                                             input_ [type_ "submit", name_ "c", value_ "New"]

                  Web.get "/assess" $ do
                          teacher <- Web.param "u"
                          student <- Web.param "i"
                          version <- Web.param "v"
                          clobber <- Web.param "c"
                          let v   = (read version) :: EqVersion
                          a       <- liftIO $ retrieveAssessment "EqDB" v student teacher
                          let as  = blankAssessment v student teacher
                              t   = toHtml teacher
                              s   = toHtml student
                              ll  = lessons $ case (clobber :: String) of
                                                   "New"  -> as
                                                   "Load" -> a
                              ls  = tbLesson <$> ll
                              tgs = nub . concat $ (toList . tags) <$> ll
                              nav n = a_ [class_ "tab", id_ n, href_ "#", onclick_ $ mconcat ["showRows('",n,"')"]]
                              tbs = nav_ . mconcat $ zipWith ($) (nav <$> tgs) (toHtml <$> tgs)
                              js  = Text.intercalate " " [ "function showRows(id) {"
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
                                                         ,   "numberRows();"
                                                         , "}"
                                                         , "function copyScore(id,val) {"
                                                         ,   "var exs = document.getElementsByName(id);"
                                                         ,   "for (i = 0; i < exs.length; i++) {"
                                                         ,     "if (exs[i].type != \"radio\") { continue; };"
                                                         ,     "exs[i].value = val;"
                                                         ,   "}"
                                                         , "}"
                                                         , "function copyAdapt(self,id) {"
                                                         ,   "var exs = document.getElementsByName(id);"
                                                         ,   "for (i = 0; i < exs.length; i++) {"
                                                         ,     "if (exs[i].type != \"checkbox\") { continue; };"
                                                         ,     "exs[i].value = self.checked == true ? \"(Nothing,Just True)\" : \"(Nothing,Just False)\";"
                                                         ,     "exs[i].checked = true;"
                                                         ,   "}"
                                                         , "}"
                                                         , "function numberRows() {"
                                                         ,   "var trs = document.getElementsByTagName(\"tr\");"
                                                         ,   "var c = 1;"
                                                         ,   "for (i = 0; i < trs.length; i++) {"
                                                         ,     "if (trs[i].style.display == \"table-row\" && trs[i].id != \"heading\") {"
                                                         ,       "trs[i].cells[0].innerHTML = c;"
                                                         ,       "c++;"
                                                         ,     "}"
                                                         ,   "}"
                                                         , "}"
                                                         , mconcat ["window.onload = function () { showRows('",head tgs,"');"]
                                                         , "};"
                                                         ]
                          Web.html . renderText $ do doctypehtml_
                                 $ do header
                                      body_ $ div_ [style_ "width: 770px; margin: auto;"]
                                          $ do let italic = span_ [style_ "font-style: italic;"]
                                               banner
                                               ol_ $ do li_ "Click on a tab to select the appropriate subtest."
                                                        li_ $ mconcat ["Using the buttons, select ",italic "1"," or ",italic "0"," to record the scores for each item in the student response booklet (protocol)."]
                                                        li_ $ mconcat ["Check the boxes for all adapted items given. The electronic scoring will automatically eliminate checks for scores of ",italic "0","."]
                                                        li_ "Click on each tab to enter scores for each subtest. You will not need to save between tabs."
                                                        li_ $ mconcat ["When finished, you may click ",italic "Save"," to temporarily save the data (optional)."]
                                                        li_ $ mconcat ["Click ",italic "Export"," to generate the report to an Excel-compatible spreadsheet. ",italic "Save as"," to your Desktop and rename the file to the student's name. You may need to adjust column and row widths inside Excel to your preference."]
                                                        li_ $ mconcat ["When finished, click ",italic "New"," to begin another assessment."]
                                               script_ js
                                               with form_ [method_ "POST", action_ "/save", enctype_ "multipart/form-data"] $ do
                                                  p_ $ do if a == as && (score <$> ll) == (score <$> (lessons as)) then "New " else ""
                                                          "Assessment by ";t;" for ";s;":"
                                                  input_ [type_ "submit", name_ "s", value_ "Save"]; " "
                                                  input_ [type_ "submit", name_ "s", value_ "New"]; " "
                                                  input_ [type_ "submit", name_ "s", value_ "Export"];
                                                  select_ [name_ "ext"] $ do option_ [value_ "csv" ] "to CSV"
                                                                             option_ [value_ "htm" ] "to HTML"
                                                                             option_ [value_ "pdf" ] "to PDF"
                                                                             option_ [value_ "rtf" ] "to RTF"
                                                                             option_ [value_ "docx"] "to Word"
                                                                             option_ [value_ "xlsx"] "to Excel"
                                                  br_ []; br_ []
                                                  tbs
                                                  table_ [style_ "margin-top: 1px; width: 770px;"] $ do
                                                       tr_ [id_ "heading"] $ do th_ "Test"
                                                                                th_ "Score"
                                                                                th_ "Adapted"
                                                                                th_ "Lesson"
                                                       mconcat $ toList ls
                                                  input_ [class_ "hidden", type_ "radio", name_ "v", value_ "Eq2", checked_]
                                                  input_ [class_ "hidden", type_ "text", name_ "u", value_ $ Text.pack teacher]
                                                  input_ [class_ "hidden", type_ "text", name_ "i", value_ $ Text.pack student]

                  Web.post "/save" $ do
                           p       <- Web.params
                           ext     <- Web.param "ext"
                           ret     <- Web.param "s"
                           version <- Web.param "v"
                           teacher <- Web.param "u"
                           student <- Web.param "i"
                           let v   = (read version) :: EqVersion
                               as  = blankAssessment v student teacher
                               scs = drop 2 $ take (length p - 3) p
                               nls = (read . Lazy.unpack . fst <$> scs) :: [(Int,Char,Int)]
                               nss = (read . Lazy.unpack . snd <$> scs) :: [(Maybe Int,Maybe Bool)]
                               nas = foldl' (\x (y,z) -> updateLesson x y z) as $ zip nls nss
                               fn  = mconcat [teacher,"_",student,".",ext]
                           na      <- liftIO $ saveAssessment "EqDB" nas
                           case (ret :: String) of
                                "Export" -> do sf <- liftIO $ saveFile nas ext
                                               Web.setHeader "Content-Type" $ case ext of
                                                   "csv"  -> "text/csv"
                                                   "htm"  -> "text/html; charset=UTF-8"
                                                   "pdf"  -> "application/pdf"
                                                   "rtf"  -> "application/rtf"
                                                   "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
                                                   "xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                                                   _      -> "text/plain"
                                               Web.setHeader "Content-Disposition" . Lazy.pack $ mconcat ["attachment; filename=",fn]
                                               Web.file $ mconcat ["exports/",fn]
                                "Save"   -> Web.redirect . Lazy.pack $ concat ["/assess?u=",teacher,"&i=",student,"&v=",version,"&c=Load"]
                                "New"    -> Web.redirect "/"

                  Web.get "/source" $ do
                          Web.html . renderText $ do doctypehtml_
                                 $ do header
                                      body_ $ do h1_ "Technologies we utilize"
                                                 p_  $ mconcat ["We use a variety of technologies in the creation of this service and would like to give credit where it is due. "
                                                               ,"To that end, below you will find a list of the technologies we use (along with links to their respective projects) and descriptions of how we use them."]
                                                 ul_ $ do li_ $ do a_ [href_ "http://www.ubuntu.com/"] "Ubuntu"
                                                                   " is a Free and open-source "
                                                                   a_ [href_ "http://www.linuxfoundation.org/"] "Linux Distribution"
                                                                   ". We use an Ubuntu virtual machine for our server to ensure stability, speed and flexibility."
                                                          li_ $ do a_ [href_ "https://sqlite.org/"] "SQLite"
                                                                   " is a powerful, but simple database. "
                                                                   "We use it to temporarily store assessment data."
                                                          li_ $ do a_ [href_ "http://www.latex-project.org/"] "LaTeX"
                                                                   " is a document processing system with unparalleled quality and performance. "
                                                                   "We use a particular version (called “xelatex”) during exporting assessments to PDF format."
                                                          li_ $ do a_ [href_ "https://www.haskell.org/"] "Haskell"
                                                                   " is a powerful and flexible Functional Programming language. "
                                                                   "We use it as our host language to create the Equals Online Assessment."
                                                          li_ $ do "A complete list of our Haskell library dependencies is available in the table below."
                                                                   table_ $ do tr_ $ do td_ "Name"; td_ "Description"; td_ "License"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/base"] "Base"
                                                                                        td_ "For the basic logic of the Haskell language"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/base-4.8.1.0/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/containers"] "Containers"
                                                                                        td_ "For performant Map and Sequence data structures"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/containers-0.5.6.3/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/text"] "Text"
                                                                                        td_ "For a performant implementation of Strngs"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/text-1.2.1.3/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/scotty"] "Scotty"
                                                                                        td_ "For quickly creating and serving web interfaces"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/scotty-0.10.2/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/transformers"] "Transformers"
                                                                                        td_ "For handling type interaction with the database"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/transformers-0.4.3.0/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/sqlite"] "SQLite"
                                                                                        td_ "For saving/reading to/from the database"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/sqlite-0.5.2.2/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/wai-extra"] "Wai-Extra"
                                                                                        td_ "For a logging middleware accessed from Scotty"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/wai-extra-3.0.10/src/LICENSE"] "MIT"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/lucid"] "Lucid"
                                                                                        td_ "For achieving type-checked HTML templating for all pages we serve"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/lucid-2.9.2/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/pandoc"] "Pandoc"
                                                                                        td_ "For exporting the assessment in Docx, HTML and RTF formats"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/pandoc-1.15.0.6/src/COPYING"] "GPLv2"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/bytestring"] "ByteString"
                                                                                        td_ "For writing and reading binary formats for assessment exportation"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/bytestring-0.10.6.0/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/utf8-string"] "UTF8-String"
                                                                                        td_ "For handling extended unicode characters gracefully"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/utf8-string-1.0.1.1/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/system-command"] "System-Command"
                                                                                        td_ "For calling out to xelatex (for the assessment PDF export"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/system-command-0.0.10/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/old-time"] "Old-Time"
                                                                                        td_ "For retrieving the time in a legacy format (for compatibility with Xlsx"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/old-time-1.1.0.3/src/LICENSE"] "BSD3"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/xlsx"] "Xlsx"
                                                                                        td_ "For exporting the assessment in Xlsx format"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/xlsx-0.1.1.1/src/LICENSE"] "MIT"
                                                                               tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/lens"] "Lens"
                                                                                        td_ "For programmatically generating worksheets in Xlsx"
                                                                                        td_ $ a_ [href_ "https://hackage.haskell.org/package/lens-4.13/src/LICENSE"] "BSD3"
                                                 h2_ "AbleNet loves Open Source!"
                                                 p_ $ do "We firmly believe that our community can make our products better!"
                                                         "So, to every user who purchases a license of Equals (and the Online Assessment), we provide a copy of the "
                                                         a_ [href_ "#"] "source code (to be linked)"
                                                         " of this tool! You can use this software under the terms of the "
                                                         a_ [href_ "#"] "GPLv3 License (to be linked)"
