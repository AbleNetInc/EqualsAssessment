module EqCommon where

import           Data.List
import           Data.Maybe
import qualified Data.Map       as Map
import           Data.Map          (Map)
import qualified Data.Sequence  as Seq
import           Data.Sequence     (Seq)

data EqVersion  = Eq2 | Eq3 deriving (Eq, Ord, Show)
type Chapter    = Int
type Section    = Char
type Name       = String
type Tag        = String
type Score      = Int
data Lesson     = Lesson { chapter :: Chapter
                         , section :: Section
                         , count   :: Int
                         , lName   :: Name
                         , tag     :: Tag
                         , score   :: Score
                         , adapted :: Bool
                         } deriving (Eq)

adaptedScore :: Lesson -> Double
adaptedScore l | score l == 0 = 0
               | adapted l    = 0.5
               | otherwise    = 1

instance Show Lesson where
    show l = concat ["(", t, ") ", c,s,o, ". ", n, ": ", a, " (", r, ")"]
           where t = tag l
                 c = show $ chapter l
                 o = show $ count l
                 s = [section l]
                 n = lName l
                 a = show $ score l
                 r = show $ adaptedScore l

data Assessment = Assessment { student :: Name
                             , ver     :: EqVersion
                             , teacher :: Name
                             , lessons :: Seq Lesson
                             }

toCSV :: Assessment -> String
toCSV a@(Assessment i v t ls) = "Teacher:," ++ t ++ "\nStudent:," ++ i
                             ++ "\nStart at:,Chapter " ++ st ++ ",("
                             ++ s ++ ")\n\n" ++ hdr
                              where st  = show $ suggestedStart a
                                    s   = show $ adaptedTotal a
                                    hdr = "Chapter,Section,Number,Lesson,Score,Adapted"

type Specifier  = (Chapter, Section, Int, Name)

newLesson :: EqVersion -> Specifier -> Tag -> Score -> Bool -> Lesson
newLesson v (c,s,o,n) t r a | vCh && vSec && vScr = (Lesson c s o n t r a)
                            | otherwise           = error "invalid fields"
                            where vCh  = c `validChapterIn` v
                                  vSec = s `validSectionIn` v
                                  vScr = r == 0 || r == 1

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
                where adj = [0,0.5,1] >>= replicate 5
scoreBounds _   = Seq.empty
