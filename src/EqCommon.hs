{-# LANGUAGE FlexibleInstances #-}

module EqCommon where

import Data.List

data Score     = Nil | Zero | One deriving (Eq)
type IsAdapted = Bool
type Name      = String
type Lesson    = (Name, Score, IsAdapted)
type Category  = (Name, [Lesson])
type Form      = (Name, [Category])
data EqVersion = Eq2 | Eq3 deriving (Eq, Show)

instance Show Score where
    show Nil  = ""
    show Zero = "0"
    show One  = "1"

showAdapted :: IsAdapted -> String
showAdapted a = if a then "Adapted" else "Unadapted"

showLesson :: Lesson -> String
showLesson (n,s,a) = intercalate "," $ [n, show s, showAdapted a]

showCategory :: Category -> String
showCategory (n,ls) = intercalate "\n" [n ++ "," ++ showLesson l | l <- ls]

toIntegral :: Integral a => Score -> a
toIntegral s | s == One  = 1
             | otherwise = 0

validLesson :: Lesson -> Bool
validLesson (_,s,a) = not $ s == Nil && a

invalidLessons :: Category -> Category
invalidLessons (n, ls) = (n, [l | l <- ls, not $ validLesson l])

validForm :: Form -> Bool
validForm (_,cs) = (== 0) . length . concat $ map (snd . invalidLessons) cs

scoreLesson :: Integral a => Lesson -> a
scoreLesson (_,s,_) = toIntegral s

rawScore :: Integral a => Category -> a
rawScore (_,ls) = sum [scoreLesson l| l <- ls]

rawAdjustment :: Integral a => Category -> a
rawAdjustment (_,ls) = genericLength $ filter (== True) adjs
                     where (_,_,adjs) = unzip3 ls

totalRaw :: Integral a => Form -> a
totalRaw (_,cs) = sum $ map rawScore cs

totalAdjustment :: Integral a => Form -> a
totalAdjustment (_,cs) = sum $ map rawAdjustment cs

totalScore :: Form -> Double
totalScore f = raw - 0.5 * adj
             where raw = fromIntegral $ totalRaw f
                   adj = fromIntegral $ totalAdjustment f

toCSV :: Form -> String
toCSV f@(n, cs) = intercalate "\n" $ header : map showCategory cs ++ [totals]
                where header = "Section,Lesson,Score,IsAdapted"
                      totals = intercalate "," $ "Total" : fmap ($ f) fns
                      fns    = [ show . totalScore
                               , show . totalRaw
                               , show . totalAdjustment
                               ]
