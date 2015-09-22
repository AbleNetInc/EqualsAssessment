module EqCommon where

import           Data.List
import           Data.Foldable        (toList)
import           Data.Maybe
import qualified Data.Map          as Map
import           Data.Map             (Map)
import qualified Data.Sequence     as Seq
import           Data.Sequence        (Seq)
import qualified Data.Text         as Text
import           Data.Text            (Text)
import           Text.Pandoc       as Doc
import           Text.Pandoc.Error    (handleError)

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

csLesson :: Lesson -> Text
csLesson l = Text.pack $ intercalate "," [s,n,c]
       where s = intercalate "." [show $ ch,[sc],show $ cnt]
             ch = chapter l
             sc = section l
             cn = count l
             n = Text.unpack . snd $ lName l
             c = show $ adaptedScore l
             cnt | ch == 11 && sc == 'E' && cn > 6 = cn - 1
                 | otherwise                       = cn

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

ltLesson :: Lesson -> String
ltLesson l@(Lesson c s o n t r a) = intercalate " & " [i,d,ars] ++ "\\\\\\hline"
       where i   = intercalate "." [show $ c,[s],show $ cnt]
             d   = Text.unpack $ snd n
             ars = show $ adaptedScore l
             cnt | c == 11 && s == 'E' && o > 6 = o - 1
                 | otherwise                    = o

toLaTeX :: Assessment -> String
toLaTeX a@(Assessment i v t ls)
      = intercalate "\n" ["\\documentclass[letterpaper]{article}"
                         ,"\\usepackage{ifxetex,longtable}"
                         ,"\\usepackage[margin=0.5in]{geometry}"
                         ,"\\begin{document}"
                         ,"\\section*{Equals Assessment Results}"
                         ,"\\noindent"
                         ,concat ["Teacher: ",Text.unpack t,"\\\\"]
                         ,concat ["Student: ",Text.unpack i,"\\\\"]
                         ,concat ["Start at Chapter ",ch," (scored ",sc,")\\\\"]
                         ,"\\ifxetex\\let\\tabular\\longtable\\let\\endtabular\\endlongtable\\fi"
                         ,"\\begin{tabular}[c]{|l|l|r|}"
                         ,"\\hline"
                         ,"Lesson & Description & Adjusted Raw Score \\\\\\hline"
                         ,intercalate "\n" $ ltLesson <$> fls l'
                         ,"\\end{tabular}"
                         ,"\\end{document}"
                         ] where l  = retrieveLesson ls (11,'E',5)
                                 l' = retrieveLesson ls (11,'E',6)
                                 (ns,na) = bottomScore l l'
                                 a' = updateLesson a (11,'E',5) (Just ns, Just na)
                                 a'' = updateLesson a' (11,'E',6) (Just 0, Just False)
                                 lls = toList . Seq.sort $ lessons a''
                                 fls Nothing = lls
                                 fls (Just lsn) = filter (/= lsn) lls
                                 ch = show $ suggestedStart a''
                                 sc = show $ adaptedTotal a''

toCSV :: Assessment -> Text
toCSV a@(Assessment i v t ls) = Text.pack $ concat [ "Teacher:,",n, "\nStudent:,", id
                                       , "\nStart at:,Chapter ",st,",(scored ",s,")\n\n"
                                       , hdr, bdy]
                              where n   = Text.unpack t
                                    id  = Text.unpack i
                                    l   = retrieveLesson ls (11,'E',5)
                                    l'  = retrieveLesson ls (11,'E',6)
                                    (ns,na) = bottomScore l l'
                                    a'  = updateLesson a (11,'E',5) (Just ns,Just na)
                                    a'' = updateLesson a' (11,'E',6) (Just 0,Just False)
                                    st  = show $ suggestedStart a''
                                    s   = show $ adaptedTotal a''
                                    lls = toList . Seq.sort $ lessons a'
                                    fls Nothing    = lls
                                    fls (Just lsn) = filter (/= lsn) lls
                                    cls = csLesson <$> (fls l')
                                    hdr = "Lesson,Description,Adjusted Raw Score\n"
                                    bdy = concat $ ((++ "\n") . Text.unpack) <$> cls

saveFile :: Assessment -> String -> IO ()
saveFile a ext = writeFile (concat [t,"_",s,".",ext]) f
       where i = handleError . readLaTeX def $ toLaTeX a
             f = case ext of
                    --"csv"  ->
                    "htm"  -> writeHtmlString def i
                    --"pdf"  ->
                    "rtf"  -> writeRTF        def i
                    --"docx" -> writeDocx       def i
                    --"xlsx" ->
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
                         sections = Map.fromList [ (Eq2, Seq.fromList ['A'..'E'])
                                                 , (Eq3, Seq.fromList ['A'..'E'])
                                                 ]

rawTotal :: Assessment -> Int
rawTotal (Assessment _ _ _ ls) = foldl (+) 0 $ score <$> ls

adaptedTotal :: Assessment -> Double
adaptedTotal (Assessment _ _ _ ls) = foldl (+) 0 $ adaptedScore <$> ls

suggestedStart :: Assessment -> Chapter
suggestedStart a@(Assessment _ v _ _) = 1 + idx ch
                                      where aScr   = adaptedTotal a
                                            ch     = Seq.findIndexL (aScr <=) b
                                            b      = scoreBounds v
                                            idx (Just c) = c
                                            idx Nothing  = 0

scoreBounds :: EqVersion -> (Seq Double)
scoreBounds Eq2 = Seq.fromList $ zipWith (+) ((27.5 *) <$> [1..12]) adj
                where adj = [0,0.5..] >>= replicate 5
scoreBounds _   = Seq.empty

updateScore :: Lesson -> Maybe Score -> Maybe Bool -> Lesson
updateScore (Lesson c s o n t _ _) (Just r') (Just a') = (Lesson c s o n t r' a')
updateScore (Lesson c s o n t r _) Nothing   (Just a') = (Lesson c s o n t r a')
updateScore (Lesson c s o n t _ a) (Just r') Nothing   = (Lesson c s o n t r' a)
updateScore (Lesson c s o n t r a) Nothing   Nothing   = (Lesson c s o n t r a)

updateLesson :: Assessment -> (Int,Char,Int) -> (Maybe Score,Maybe Bool) -> Assessment
updateLesson a@(Assessment n v t ls) (c,s,o) (r,b) = Assessment n v t $ newLs idx
           where l    = newLesson v (c,s,o,(Text.pack "",Text.pack "")) Seq.empty 0 False
                 idx  = Seq.elemIndexL l ls
                 newL i         = updateScore (Seq.index ls i) r b
                 newLs Nothing  = ls
                 newLs (Just i) = Seq.update i (newL i) ls
