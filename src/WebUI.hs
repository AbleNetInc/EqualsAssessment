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
                                          , "</head>"
                                          , "<h1>Assessment by ",t," for ",s,":</h1>"
                                          , "<table>"
                                          , mconcat trs
                                          , "</table>"
                                          , "</html>"
                                          ]
