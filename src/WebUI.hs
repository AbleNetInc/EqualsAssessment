{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module WebUI where

import           EqCommon
import           EqLessons
import           EqSQL
import qualified Web.Scotty                            as Web
import           Lucid.Base
import           Lucid.Html5
import qualified Data.Text                             as Text
import           Data.Text                                (Text)
import qualified Data.Text.Lazy                        as Lazy
import           Data.Foldable                            (toList)
import           Data.List                                (nub, foldl')
import           Control.Monad.IO.Class                   (liftIO)
import           Network.Wai.Middleware.RequestLogger     (logStdoutDev)

tbLesson :: Lesson -> Html ()
tbLesson l = tr_ [class_ c, style_ st] $ do td_ ""; td_ s; td_ [style_ "text-align: center;"] a; td_ n
       where n = if m == "(1,'C',4)" then adj else toHtml . fst $ lName l
             st = "display: none;"
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
             s = do input_ [type_ "radio", name_ m, oc !! 0, value_ "(Just (-1),Nothing)", ch !! 0, id_ "blank"]
                    label_ [for_ "blank", style_ "margin-right: 15px;"] "blank"
                    input_ [type_ "radio", name_ m, oc !! 1, value_ "(Just 0,Nothing)",    ch !! 1, id_ "zero"]
                    label_ [for_ "zero", style_ "margin-right: 15px;"] "0"
                    input_ [type_ "radio", name_ m, oc !! 2, value_ "(Just 1,Nothing)",    ch !! 2, id_ "one"]
                    label_ [for_ "one"] "1"
             a = input_ [type_ "checkbox", name_ m, cc, value_ "(Nothing,Just True)",  b]
             b = if adapted l then checked_ else alt_ ""
             hidden = m == "(1,'C',5)" || m == "(9,'A',4)"

css :: Text
css = Text.intercalate " "
    [ "@font-face {"
    ,    "font-family: 'Trade Gothic W01 Light';"
    ,    "src: url(\"/assets/tgw01l.woff\") format(\"woff\");"
    , "}"
    , "@font-face {"
    ,    "font-family: MetaSerifBk;"
    ,    "src: url(\"/assets/msb.woff\") format(\"woff\");"
    , "}"
    , "body {"
    ,    "font-size: 15px;"
    ,    "font-family: 'Trade Gothic W01 Light', sans;"
    ,    "color: #685e56;"
    ,    "margin: 0;"
    ,    "padding: 0;"
    , "}"
    , "h1 {"
    ,    "font-family: MetaSerifBk, serif;"
    ,    "font-size: 47px;"
    ,    "color: #a2789c;"
    , "}"
    , "a {"
    ,    "text-decoration: none;"
    , "}"
    , "a.inline {"
    ,    "color: #dd7b00;"
    , "}"
    , "a.footer {"
    ,    "color: #685e56;"
    , "}"
    , "a.footer:hover {"
    ,    "color: #dd7b00;"
    ,    "text-decoration: underline;"
    , "}"
    , "a.inline:hover {"
    ,    "text-decoration: underline;"
    ,    "color: #dd7b00;"
    , "}"
    , "div.main {"
    ,    "width: 60%;"
    ,    "height: 100%;"
    ,    "margin: auto;"
    , "}"
    , "div.lscroll {"
    ,    "background-image: url(\"/assets/ml.png\");"
    ,    "background-size: contain;"
    ,    "width: 26px;"
    ,    "height: 41px !important;"
    ,    "vertical-align: bottom;"
    ,    "line-height: 35px;"
    ,    "margin-right: -36px;"
    ,    "float: left;"
    ,    "position: relative;"
    ,    "z-index: 1;"
    , "}"
    , "div.lscroll:hover {"
    ,    "background-image: url(\"/assets/ml_h.png\");"
    , "}"
    , "div.rscroll {"
    ,    "background-image: url(\"/assets/mr.png\");"
    ,    "background-size: contain;"
    ,    "width: 26px;"
    ,    "height: 41px !important;"
    ,    "vertical-align: bottom;"
    ,    "line-height: 35px;"
    ,    "float: right;"
    ,    "margin-left: -36px;"
    , "}"
    , "div.rscroll:hover {"
    ,    "background-image: url(\"/assets/mr_h.png\");"
    , "}"
    , "footer {"
    ,    "position: absolute;"
    ,    "bottom: 20px;"
    ,    "right: 20px;"
    , "}"
    , "table, th, td {"
    ,    "border: 1px solid #b4afab;"
    ,    "border-collapse: collapse;"
    ,    "padding: 5px;"
    , "}"
    , "input {"
    ,    "margin-right: 10px;"
    ,    "cursor: pointer;"
    ,    "outline: none;"
    , "}"
    , ".itext {"
    ,    "border: 2px solid #c5c0ba;"
    ,    "background: #fff none repeat scroll 0% 0%;"
    ,    "border-radius: 25px;"
    ,    "height: 45px;"
    ,    "width: 35%;"
    ,    "margin-bottom: 10px;"
    ,    "padding: 0px 25px;"
    , "}"
    , "select {"
    ,    "border: 1px solid #c5c0bc;"
    ,    "padding: 10px 30px 10px 15px !important;"
    ,    "border-radius: 25px;"
    ,    "height: 45px;"
    ,    "background: transparent url(\"assets/ui-select-arrow.jpg\") no-repeat scroll right 10px center / 16px 8px !important;"
    ,    "-moz-appearance: none;"
    , "}"
    , ".action {"
    ,    "border: 1px solid #963821;"
    ,    "padding: 10px 15px !important;"
    ,    "border-radius: 25px;"
    ,    "height: 45px;"
    ,    "text-transform: uppercase;"
    ,    "background: #963821 none repeat scroll 0% 0%;"
    ,    "color: #fff;"
    , "}"
    , ".action:hover {"
    ,    "background: #dd7b00 none repeat scroll 0% 0%;"
    ,    "border: 1px solid #dd7b00;"
    ,    "color: #fff;"
    , "}"
    , ".button {"
    ,    "border: 1px solid #dd7b00;"
    ,    "padding: 10px 15px !important;"
    ,    "border-radius: 25px;"
    ,    "height: 45px;"
    ,    "text-transform: uppercase;"
    ,    "background: #fff none repeat scroll 0% 0%;"
    ,    "color: #665d56;"
    , "}"
    , ".button:hover {"
    ,    "background: #dd7b00 none repeat scroll 0% 0%;"
    ,    "color: #fff;"
    , "}"
    , "div.tabs {"
    ,    "overflow: hidden;"
    ,    "width: 95%;"
    ,    "white-space: nowrap;"
    ,    "float: left;"
    ,    "padding-top: 6px;"
    ,    "padding-left: 36px;"
    , "}"
    , ".tab {"
    ,    "outline: none;"
    ,    "background: #665d56 none repeat scroll 0% 0%;"
    ,    "color: #fff;"
    ,    "margin-right: 10px;"
    ,    "padding: 13px 15px 10px 15px !important;"
    ,    "border-radius: 20px 20px 0px 0px;"
    ,    "line-height: 35px;"
    ,    "height: 45px !important;"
    ,    "white-space: nowrap;"
    ,    "border: 1px solid #bbb;"
    ,    "border-color: #b4afab #b4afab transparent;"
    ,    "text-decoration: none;"
    ,    "text-transform: uppercase;"
    , "}"
    , ".selected {"
    ,    "background: #fff none repeat scroll 0% 0%;"
    ,    "color: #665d56;"
    , "}"
    , ".hidden {"
    ,    "display: none;"
    , "}"
    ]

header :: Html ()
header = head_ $ do
     meta_ [charset_ "UTF-8"]
     style_ css
     title_ "Equals Assessment"
     link_ [href_ "https://www.ablenetinc.com/media/favicon/default/favicon.ico", rel_ "icon", type_ "image/x-icon"]

banner :: Html ()
banner = do img_ [ style_ "margin-top: 20px; margin-left: 42.75%; width: 100px;"
                 , src_ "assets/banner.jpg"]
            h1_ "Equals Online Assessment"
            p_ [style_ "color: #963821;"] $
               mconcat ["Note: While you may save your data, ",all'," assessment data is deleted each night at midnight (Central Time)"]
            where all' = span_ [style_ "font-style: italic;"] "all"

footer :: Html ()
footer = do footer_ $ do a_ [href_ "/source", class_ "footer"] "Technologies we rely on"

runWebServer :: Int -> IO ()
runWebServer pnum = Web.scotty pnum $ do
                  Web.middleware logStdoutDev

                  Web.get "/" $ do
                          Web.html . renderText $ do doctypehtml_
                                 $ do header
                                      body_ $ div_ [class_ "main"] $ do
                                          let italic = span_ [style_ "font-style: italic;"]
                                          banner
                                          p_ "To get started, complete the assessment and fill out the paper copy of the student response booklet (protocol). Then:"
                                          ol_ $ do li_ $ mconcat ["Fill out the ",italic "Username"," field with your name."]
                                                   li_ $ mconcat ["Enter a ",italic "Student ID"," number in case you need to leave and return to finish scoring later in the day. Only numbers are valid."]
                                                   li_ $ mconcat ["Click ",italic "New"," to enter data for the student."]
                                          p_ $ mconcat ["To retrieve data saved earlier in the day, enter the ",italic "Username"," and ",italic "Student ID"," and click ",italic "Load","."]
                                          with form_ [method_ "GET", action_ "/assess"] $ do
                                             p_ $ do input_ [class_ "itext", type_ "text", name_ "u", required_ "", placeholder_ "Username"]
                                                     br_ []
                                                     input_ [class_ "itext", type_ "number", min_ "0", name_ "i", required_ "", placeholder_ "Student ID"]
                                             input_ [class_ "hidden", type_ "radio", name_ "v", value_ "Eq2", checked_]
                                             input_ [class_ "button", type_ "submit", name_ "c", value_ "Load"]
                                             input_ [class_ "button", type_ "submit", name_ "c", value_ "New"]
                                      footer

                  Web.get "/assets/:file" $ do
                          f <- Web.param "file"
                          Web.file $ mconcat ["assets/",f]

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
                              nl  = div_ [class_ "lscroll", onclick_ "sLeft('tabbar');"] ""
                              nr  = div_ [class_ "rscroll", onclick_ "sRight('tabbar');"] ""
                              tbs = nav_ $ do nl; div_ [class_ "tabs", id_ "tabbar"] (mconcat $ zipWith ($) (nav <$> tgs) (toHtml <$> tgs)); nr
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
                                                         ,     "if (as[i].className != \"footer\") { as[i].className = \"tab\"; }"
                                                         ,   "}"
                                                         ,   "document.getElementById(id).className += \" selected\";"
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
                                                         , "function sLeft(id) {"
                                                         ,   "document.getElementById(id).scrollLeft -= 100;"
                                                         , "}"
                                                         , "function sRight(id) {"
                                                         ,   "document.getElementById(id).scrollLeft += 100;"
                                                         , "}"
                                                         , "(function (i,s,o,g,r,a,m) {"
                                                         ,    "i['GoogleAnalyticsObject']=r;i[r]=i[r]||function () {"
                                                         ,       "(i[r].q=i[r].q||[]).push(arguments)"
                                                         ,    "},i[r].l=1*new Date();a=s.createElement(o),"
                                                         ,    "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
                                                         , "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
                                                         , "ga('create', 'UA-1851794-1', 'auto');"
                                                         , "ga('send', 'pageview');"
                                                         , mconcat ["window.onload = function () { showRows('",head tgs,"');"]
                                                         , "};"
                                                         ]
                          Web.html . renderText $ do doctypehtml_
                                 $ do header
                                      body_ $ div_ [class_ "main"]
                                          $ do let italic = span_ [style_ "font-style: italic;"]
                                               banner
                                               ol_ $ do li_ "Click on a tab to select the appropriate subtest."
                                                        li_ $ mconcat ["Using the buttons, select ",italic "1"," or ",italic "0"," to record the scores for each item in the student response booklet (protocol)."]
                                                        li_ $ mconcat ["Check the boxes for all adapted items given. The electronic scoring will automatically eliminate checks for scores of ",italic "0","."]
                                                        li_ "Click on each tab to enter scores for each subtest. You will not need to save between tabs."
                                                        li_ $ mconcat ["When finished, you may click ",italic "Save"," to temporarily save the data (optional)."]
                                                        li_ $ mconcat ["Click ",italic "Export"," to generate the report to your preferrred format. ",italic "Save as"," to your Desktop and rename the file to the student's name."]
                                                        li_ $ mconcat ["When finished, click ",italic "New"," to begin another assessment."]
                                               script_ js
                                               with form_ [method_ "POST", action_ "/save", enctype_ "multipart/form-data"] $ do
                                                  p_ $ do if a == as && (score <$> ll) == (score <$> (lessons as)) then "New " else ""
                                                          "Assessment by ";t;" for ";s;":"
                                                  input_ [class_ "button", type_ "submit", name_ "s", value_ "Save"]; " "
                                                  input_ [class_ "button", type_ "submit", name_ "s", value_ "New"]; " "
                                                  div_ [style_ "float: right;"] $ do
                                                      input_ [class_ "action", type_ "submit", name_ "s", value_ "Export"];
                                                      select_ [name_ "ext"] $ do option_ [value_ "xlsx"] "Excel"
                                                                                 option_ [value_ "csv" ] "CSV"
                                                                                 option_ [value_ "htm" ] "HTML"
                                                                                 option_ [value_ "pdf" ] "PDF"
                                                                                 option_ [value_ "rtf" ] "RTF"
                                                                                 option_ [value_ "docx"] "Word"
                                                  br_ []; br_ []
                                                  tbs
                                                  table_ [style_ "margin-top: 1px; width: 100%;"] $ do
                                                       tr_ [id_ "heading"] $ do th_ "Test"
                                                                                th_ "Score"
                                                                                th_ "Adapted"
                                                                                th_ "Lesson"
                                                       mconcat $ toList ls
                                                  input_ [class_ "hidden", type_ "radio", name_ "v", value_ "Eq2", checked_]
                                                  input_ [class_ "hidden", type_ "text", name_ "u", value_ $ Text.pack teacher]
                                                  input_ [class_ "hidden", type_ "text", name_ "i", value_ $ Text.pack student]
                                      footer

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
                                      body_ $ do div_ [class_ "main"] $ do
                                                    banner
                                                    h2_ "Technologies we utilize"
                                                    p_  $ mconcat ["We use a variety of technologies in the creation of this service and would like to give credit where it is due. "
                                                                  ,"To that end, below you will find a list of the technologies we use (along with links to their respective projects) and descriptions of how we use them."]
                                                    ul_ $ do li_ $ do a_ [href_ "http://www.ubuntu.com/", class_ "inline"] "Ubuntu"
                                                                      " is a Free and open-source "
                                                                      a_ [href_ "http://www.linuxfoundation.org/", class_ "inline"] "Linux Distribution"
                                                                      ". We use an Ubuntu virtual machine for our server to ensure stability, speed and flexibility."
                                                             li_ $ do a_ [href_ "https://sqlite.org/", class_ "inline"] "SQLite"
                                                                      " is a powerful, but simple database. "
                                                                      "We use it to temporarily store assessment data."
                                                             li_ $ do a_ [href_ "http://www.latex-project.org/", class_ "inline"] "LaTeX"
                                                                      " is a document processing system with unparalleled quality and performance. "
                                                                      "We use a particular version (called “xelatex”) during exporting assessments to PDF format."
                                                             li_ $ do a_ [href_ "https://www.git-scm.com/", class_ "inline"] "Git"
                                                                      " is a source-control management software with fabulous performance and functionality. "
                                                                      "We use it to keep track of all our changes made to the software."
                                                             li_ $ do a_ [href_ "https://www.haskell.org/", class_ "inline"] "Haskell"
                                                                      " is a powerful and flexible Functional Programming language. "
                                                                      "We use it as our host language to create the Equals Online Assessment."
                                                             li_ $ do "A complete list of our Haskell library dependencies is available in the table below."
                                                                      table_ $ do tr_ $ do td_ "Name"; td_ "Description"; td_ "License"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/base", class_ "inline"] "Base"
                                                                                           td_ "For the basic logic of the Haskell language"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/base-4.8.1.0/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/containers", class_ "inline"] "Containers"
                                                                                           td_ "For performant Map and Sequence data structures"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/containers-0.5.6.3/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/text", class_ "inline"] "Text"
                                                                                           td_ "For a performant implementation of Strngs"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/text-1.2.1.3/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/scotty", class_ "inline"] "Scotty"
                                                                                           td_ "For quickly creating and serving web interfaces"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/scotty-0.10.2/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/transformers", class_ "inline"] "Transformers"
                                                                                           td_ "For handling type interaction with the database"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/transformers-0.4.3.0/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/sqlite", class_ "inline"] "SQLite"
                                                                                           td_ "For saving/reading to/from the database"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/sqlite-0.5.2.2/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/wai-extra", class_ "inline"] "Wai-Extra"
                                                                                           td_ "For a logging middleware accessed from Scotty"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/wai-extra-3.0.10/src/LICENSE", class_ "inline"] "MIT"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/lucid", class_ "inline"] "Lucid"
                                                                                           td_ "For achieving type-checked HTML templating for all pages we serve"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/lucid-2.9.2/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/pandoc", class_ "inline"] "Pandoc"
                                                                                           td_ "For exporting the assessment in Docx, HTML and RTF formats"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/pandoc-1.15.0.6/src/COPYING", class_ "inline"] "GPLv2"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/bytestring", class_ "inline"] "ByteString"
                                                                                           td_ "For writing and reading binary formats for assessment exportation"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/bytestring-0.10.6.0/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/utf8-string", class_ "inline"] "UTF8-String"
                                                                                           td_ "For handling extended unicode characters gracefully"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/utf8-string-1.0.1.1/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/system-command", class_ "inline"] "System-Command"
                                                                                           td_ "For calling out to xelatex (for the assessment PDF export"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/system-command-0.0.10/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/old-time", class_ "inline"] "Old-Time"
                                                                                           td_ "For retrieving the time in a legacy format (for compatibility with Xlsx"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/old-time-1.1.0.3/src/LICENSE", class_ "inline"] "BSD3"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/xlsx", class_ "inline"] "Xlsx"
                                                                                           td_ "For exporting the assessment in Xlsx format"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/xlsx-0.1.1.1/src/LICENSE", class_ "inline"] "MIT"
                                                                                  tr_ $ do td_ $ a_ [href_ "https://hackage.haskell.org/package/lens", class_ "inline"] "Lens"
                                                                                           td_ "For programmatically generating worksheets in Xlsx"
                                                                                           td_ $ a_ [href_ "https://hackage.haskell.org/package/lens-4.13/src/LICENSE", class_ "inline"] "BSD3"
                                                    h2_ "AbleNet loves Open Source!"
                                                    p_ $ do "We firmly believe that our community can make our products better!"
                                                            " So, to every user who purchases a license of Equals (and the Online Assessment), we provide a copy of the "
                                                            a_ [href_ "#", class_ "inline"] "source code (to be linked)"
                                                            " of this tool! You can use this software under the terms of the "
                                                            a_ [href_ "#", class_ "inline"] "GPLv3 License (to be linked)"
