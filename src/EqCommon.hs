module EqCommon where

import           Data.List
import           Data.Foldable        (toList)
import           Data.Maybe
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.UTF8 as U8
import qualified Data.Map             as Map
import           Data.Map                (Map)
import qualified Data.Sequence        as Seq
import           Data.Sequence           (Seq)
import qualified Data.Text            as Text
import           Data.Text               (Text)
import           Text.Pandoc          as Doc
import           Text.Pandoc.Error       (handleError)
import           System.Command          (rawSystem, inDirectory')

data EqVersion  = Eq2 | Eq3 deriving (Eq, Ord, Show, Read)
type Chapter    = Int
type Section    = Char
type Name       = Text
type Tag        = Text
type Score      = Int
data Lesson     = Lesson { chapter :: Chapter
                         , section :: Section
                         , count   :: Int
                         , lName   :: (Name,Name)
                         , tags    :: (Seq Tag)
                         , score   :: Score
                         , adapted :: Bool
                         } deriving (Show, Read)

instance Eq Lesson where
    l == l' = sameCh && sameSc && sameCo
            where sameCh = chapter l == chapter l'
                  sameSc = section l == section l'
                  sameCo = count   l == count   l'

instance Ord Lesson where
    compare l@(Lesson c s o _ _ _ _) l'@(Lesson c' s' o' _ _ _ _)
          | c < c'                       = LT
          | c == c' && s < s'            = LT
          | c == c' && s == s' && o < o' = LT
          | l == l'                      = EQ
          | otherwise                    = GT

adaptedScore :: Lesson -> Double
adaptedScore l | score l /= 1 = 0
               | adapted l    = 0.5
               | otherwise    = 1

data Assessment = Assessment { student :: Name
                             , ver     :: EqVersion
                             , teacher :: Name
                             , lessons :: Seq Lesson
                             } deriving (Eq)

bottomScore :: Maybe Lesson -> Maybe Lesson -> (Score, Bool)
bottomScore Nothing  Nothing   = (0, False)
bottomScore Nothing  (Just l') = (score l', adapted l')
bottomScore (Just l) Nothing   = (score l,  adapted l)
bottomScore (Just l) (Just l') | al < al'  = (score l,  adapted l)
                               | otherwise = (score l', adapted l')
                               where al  = adaptedScore l
                                     al' = adaptedScore l'

retrieveLesson :: Seq Lesson -> (Chapter, Section, Int) -> Maybe Lesson
retrieveLesson ls (c,s,o) | found     = Just l
                          | otherwise = Nothing
                          where l'    = Lesson c s o (Text.empty,Text.empty) Seq.empty 0 False
                                idx   = Seq.elemIndexL l' ls
                                found = idx /= Nothing
                                l     = Seq.index ls $ fromJust idx

csLesson :: Lesson -> String
csLesson l = intercalate "," [s,n,c]
       where s = intercalate "." [show $ ch,[sc],show $ cnt]
             ch = chapter l
             sc = section l
             cn = count l
             n = Text.unpack . snd $ lName l
             c = show $ adaptedScore l
             cnt | ch == 11 && sc == 'E' && cn > 6 = cn - 1
                 | otherwise                       = cn

ltLesson :: Lesson -> String
ltLesson l@(Lesson c s o n t r a) = intercalate " & " [i,d,ars] ++ "\\\\\\hline"
       where i   = intercalate "." [show $ c,[s],show $ cnt]
             d   = Text.unpack $ snd n
             ars = show $ adaptedScore l
             cnt | c == 11 && s == 'E' && o > 6 = o - 1
                 | otherwise                    = o

makeExceptions :: Assessment -> Assessment
makeExceptions a@(Assessment i v t ls) | ver a == Eq2 = nA
                                       | otherwise    = a
             where l  = retrieveLesson ls (11,'E',5)
                   l' = retrieveLesson ls (11,'E',6)
                   (ns,na) = bottomScore l l'
                   a' = updateLesson a (11,'E',5) (Just ns, Just na)
                   nls Nothing    = lessons a'
                   nls (Just lsn) = Seq.filter (/= lsn) $ lessons a'
                   nA = Assessment i v t $ nls l'

toLaTeX :: Assessment -> String
toLaTeX a = unlines ["\\documentclass[letterpaper]{article}"
                    ,"\\usepackage{ifxetex,longtable}"
                    ,"\\usepackage[margin=0.5in]{geometry}"
                    ,"\\begin{document}"
                    ,"\\section*{Equals Assessment Results}"
                    ,"\\noindent"
                    ,concat ["Teacher: ",t,"\\\\"]
                    ,concat ["Student: ",i,"\\\\"]
                    ,concat ["Start at Chapter ",ch
                            ," (Adjusted Raw Score: ",sc,")\\\\"]
                    ,concat ["\\ifxetex\\let\\tabular\\longtable"
                            ,"\\let\\endtabular\\endlongtable\\fi"]
                    ,"\\begin{tabular}[c]{|l|l|r|}"
                    ,"\\hline"
                    ,"Lesson & Description & Score\\\\\\hline"
                    ,unlines $ ltLesson <$> sl
                    ,"\\end{tabular}"
                    ,"\\end{document}"
                    ] where na = makeExceptions a
                            ls = lessons na
                            sl = toList $ Seq.sort ls
                            ch = show $ suggestedStart na
                            sc = show $ adaptedTotal na
                            i  = Text.unpack $ student na
                            t  = Text.unpack $ teacher na

toCSV :: Assessment -> String
toCSV a = unlines [ concat ["Teacher:,", t]
                  , concat ["Student:,", i]
                  , concat ["Start at:,Chapter ",ch
                           ," (Adjusted Raw Score: ",sc,")\n"]
                  , "Lesson,Description,Score"
                  , unlines $ csLesson <$> sl
                  ]
        where na = makeExceptions a
              ls = lessons na
              sl = toList $ Seq.sort ls
              ch = show $ suggestedStart na
              sc = show $ adaptedTotal na
              i  = Text.unpack $ student na
              t  = Text.unpack $ teacher na

writeXLSX :: WriterOptions -> Pandoc -> IO DBL.ByteString
writeXLSX _ p = return . DBL.fromStrict $ U8.fromString "skeleton"

saveFile :: Assessment -> String -> IO ()
saveFile a ext | ext == "docx" = writeDocx def i >>= DBL.writeFile n
               | ext == "pdf"  = do writeFile ("exports/" ++ n') f
                                    inDirectory' "exports" $ rawSystem "xelatex" [n']
                                    return ()
               -- | ext == "xlsx" = writeXLSX def i >>= DBL.writeFile n
               | otherwise     = writeFile n f
       where i = handleError . readLaTeX def $ toLaTeX a
             n = concat ["exports/",t,"_",s,".",ext]
             n' = concat [t,"_",s,".tex"]
             f = case ext of
                    "csv"  -> toCSV a
                    "htm"  -> writeHtmlString def i
                    "rtf"  -> writeRTF        def i
                    "pdf"  -> toLaTeX a
             t = Text.unpack $ teacher a
             s = Text.unpack $ student a

type Specifier  = (Chapter, Section, Int, (Name,Name))

newLesson :: EqVersion -> Specifier -> (Seq Tag) -> Score -> Bool -> Lesson
newLesson v (c,s,o,n) t r a | not vCh   = error "Invalid Chapter"
                            | not vSec  = error "Invalid Section"
                            | not vScr  = error "Invalid Score"
                            | otherwise = (Lesson c s o n t r a)
                            where vCh  = c `validChapterIn` v
                                  vSec = s `validSectionIn` v
                                  vScr = r `elem` [(-1)..1]

validChapterIn :: Chapter -> EqVersion -> Bool
validChapterIn c v = (Seq.elemIndexL c cList) /= Nothing
                   where cList    = fromJust $ Map.lookup v chapters
                         chapters = Map.fromList [ (Eq2, Seq.fromList [1..12])
                                                 , (Eq3, Seq.fromList [1..10])
                                                 ]

validSectionIn :: Section -> EqVersion -> Bool
validSectionIn s v = (Seq.elemIndexL s sList) /= Nothing
                   where sList    = fromJust $ Map.lookup v sections
                         sections = Map.fromList
                                [ (Eq2, Seq.fromList ['A'..'E'])
                                , (Eq3, Seq.fromList ['A'..'E'])
                                ]

rawTotal :: Assessment -> Int
rawTotal a = foldl1 (+) $ score <$> (lessons a)

adaptedTotal :: Assessment -> Double
adaptedTotal a = foldl1 (+) $ adaptedScore <$> (lessons a)

suggestedStart :: Assessment -> Chapter
suggestedStart a = 1 + idx ch
                 where aScr   = adaptedTotal a
                       ch     = Seq.findIndexL (aScr <=) b
                       b      = scoreBounds $ ver a
                       idx (Just c) = c
                       idx Nothing  = 0

scoreBounds :: EqVersion -> (Seq Double)
scoreBounds Eq2 = Seq.fromList $ zipWith (+) ((27.5 *) <$> [1..12]) adj
                where adj = [0,0.5..] >>= replicate 5
scoreBounds _   = Seq.empty

updateScore :: Lesson -> Maybe Score -> Maybe Bool -> Lesson
updateScore (Lesson c s o n t _ _) (Just r') (Just a') = Lesson c s o n t r' a'
updateScore (Lesson c s o n t r _) Nothing   (Just a') = Lesson c s o n t r  a'
updateScore (Lesson c s o n t _ a) (Just r') Nothing   = Lesson c s o n t r' a
updateScore (Lesson c s o n t r a) Nothing   Nothing   = Lesson c s o n t r  a

updateLesson :: Assessment -> (Int,Char,Int) -> (Maybe Score,Maybe Bool) -> Assessment
updateLesson a@(Assessment n v t ls) (c,s,o) (r,b) = Assessment n v t $ newLs idx
           where l    = newLesson v (c,s,o,(Text.pack "",Text.pack "")) Seq.empty 0 False
                 idx  = Seq.elemIndexL l ls
                 newL i         = updateScore (Seq.index ls i) r b
                 newLs Nothing  = ls
                 newLs (Just i) = Seq.update i (newL i) ls
