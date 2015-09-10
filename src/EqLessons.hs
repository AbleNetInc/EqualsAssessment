module EqLessons where

import           EqCommon
import qualified Data.Map       as Map
import           Data.Map          (Map)
import qualified Data.Sequence  as Seq
import           Data.Sequence     (Seq)
import qualified Data.Text      as Text
import           Data.Text         (Text)

blankAssessment :: EqVersion -> String -> String -> Assessment
blankAssessment v s t = Assessment (Text.pack s) v (Text.pack t) . ls $ Map.lookup v lessonSets
                      where ls (Just s) = s
                            ls Nothing  = Seq.empty

updateScore :: Lesson -> Maybe Score -> Maybe Bool -> Lesson
updateScore (Lesson c s o n t _ _) (Just r') (Just a') = (Lesson c s o n t r' a')
updateScore (Lesson c s o n t r _) Nothing   (Just a') = (Lesson c s o n t r a')
updateScore (Lesson c s o n t _ a) (Just r') Nothing   = (Lesson c s o n t r' a)
updateScore (Lesson c s o n t r a) Nothing   Nothing   = (Lesson c s o n t r a)

updateLesson :: Assessment -> (Int,Char,Int) -> (Maybe Score,Maybe Bool) -> Assessment
updateLesson a@(Assessment n v t ls) (c,s,o) (r,b) = Assessment n v t $ newLs idx
           where l    = newLesson v (c,s,o,Text.pack "") Seq.empty 0 False
                 idx  = Seq.elemIndexL l ls
                 newL i         = updateScore (Seq.index ls i) r b
                 newLs Nothing  = ls
                 newLs (Just i) = Seq.update i (newL i) ls

taggedBlankLessons :: EqVersion -> Chapter -> Section -> [(Int,Name)] -> [Tag] -> [Lesson]
taggedBlankLessons v c s ns t = [newLesson v (c,s,o,n) (Seq.fromList t) (-1) False | (o,n) <- ns]

lessonSets :: Map EqVersion (Seq Lesson)
lessonSets = Map.fromList
           [(Eq2, Seq.fromList $ concat
             [ taggedBlankLessons Eq2 c s (zip [n..] (Text.pack <$> l)) (Text.pack <$> t)
             | (c,s,n,l,t) <- [( 1,'A',1,ae2,["Attending & Exploring"])
                              ,( 1,'B',1,pt2,["Patterns & Algebra"])
                              ,( 3,'B',1,pu2,["Patterns & Algebra"])
                              ,( 8,'D',1,np2,["Patterns & Algebra"])
                              ,(10,'C',1,ma2,["Patterns & Algebra"])
                              ,( 1,'C',1,ms2,["Data Analysis & Probability"])
                              ,( 3,'D',1,gp2,["Data Analysis & Probability"])
                              ,( 3,'E',1,pb2,["Data Analysis & Probability"])
                              ,( 6,'A',1,cd2,["Data Analysis & Probability"])
                              ,(10,'A',1,lp2,["Data Analysis & Probability"])
                              ,(10,'B',1,pc2,["Data Analysis & Probability"])
                              ,( 1,'D',1,aa2,["Geometry"])
                              ,( 5,'D',1,gs2,["Geometry"])
                              ,( 9,'B',1,dc2,["Geometry"])
                              ,( 9,'C',1,ap2,["Geometry"])
                              ,( 9,'D',1,ds2,["Geometry"])
                              ,( 1,'D',7,am2,["Measurement"])
                              ,( 3,'C',1,ca2,["Measurement"])
                              ,( 5,'B',1,ti2,["Measurement"])
                              ,( 6,'C',1,mt2,["Measurement"])
                              ,( 8,'E',1,tm2,["Measurement"])
                              ,( 9,'A',1,wl2,["Measurement"])
                              ,(11,'E',1,mg2,["Measurement"])
                              ,( 2,'A',1,c12,["Numbers & Operations"])
                              ,( 2,'B',1,c32,["Numbers & Operations"])
                              ,( 2,'C',1,c52,["Numbers & Operations"])
                              ,( 2,'D',1,zm2,["Numbers & Operations"])
                              ,( 2,'E',1,c72,["Numbers & Operations"])
                              ,( 3,'A',1,on2,["Numbers & Operations"])
                              ,( 4,'A',1,ct2,["Numbers & Operations"])
                              ,( 4,'B',1,as2,["Numbers & Operations"])
                              ,( 4,'C',1,mm2,["Numbers & Operations"])
                              ,( 5,'A',1,c22,["Numbers & Operations"])
                              ,( 5,'C',1,cn2,["Numbers & Operations"])
                              ,( 6,'B',1,n52,["Numbers & Operations"])
                              ,( 7,'A',1,a72,["Numbers & Operations"])
                              ,( 7,'B',1,sa2,["Numbers & Operations"])
                              ,( 7,'C',1,a32,["Numbers & Operations"])
                              ,( 7,'D',1,dn2,["Numbers & Operations"])
                              ,( 8,'A',1,dp2,["Numbers & Operations"])
                              ,( 8,'B',1,re2,["Numbers & Operations"])
                              ,( 8,'C',1,ln2,["Numbers & Operations"])
                              ,(11,'A',1,md2,["Numbers & Operations"])
                              ,(11,'B',1,sm2,["Numbers & Operations"])
                              ,(11,'C',1,dm2,["Numbers & Operations"])
                              ,(11,'D',1,sd2,["Numbers & Operations"])
                              ,(12,'A',1,fw2,["Numbers & Operations"])
                              ,(12,'B',1,cf2,["Numbers & Operations"])
                              ,(12,'C',1,af2,["Numbers & Operations"])
                              ,(12,'D',1,fd2,["Numbers & Operations"])
             ]])
           , (Eq3, Seq.empty)]
           where ae2 = [ "attend to math materials and instruction"
                       , "touch math manipulatives"
                       , "explore math manipulatives"
                       , "choose a song or book for enjoyment"
                       , "understand cause and effect"
                       , "imitate simple problem solving activities"
                       ]
                 pt2 = [ "tolerance of kinesthetic/sound patterns"
                       , "imitate kinesthetic/sound pattern"
                       , "repeat a daily routine"
                       , "knowledge of a seasonal event"
                       , "anticipate scheduled event"
                       ]
                 pu2 = [ "match ABAB patterns"
                       , "duplicate ABAB patterns in 2 ways"
                       , "extend ABAB patterns"
                       , "describe ABAB patterns"
                       , "record ABAB patterns"
                       , "count number of units in a pattern"
                       , "compare equivalent patterns"
                       ]
                 np2 = [ "duplicate an ABBABB pattern"
                       , "extend an ABBABB pattern"
                       , "skip count by 5's"
                       , "skip count by 2's"
                       , "identify odd and even numbers"
                       , "determine missing unit in a pattern"
                       , "use number patterns - missing number"
                       ]
                 ma2 = [ "use notation for an equivalent expression"
                       , "solve an equation with a missing addend"
                       , "solve an equation with a variable"
                       , "identify equivalent and equal sets"
                       , "extend number pattern with constant increment"
                       , "use a table to determine missing cost"
                       , "describe number pattern in a table"
                       ]
                 ms2 = [ "match duplicate objects"
                       , "match objects by color attribute"
                       , "sort objects by color attribute"
                       , "identify primary colors"
                       , "identify secondary colors" -- Exception!
                       , "sort objects by size attribute"
                       , "find object with size or color attribute"
                       ]
                 gp2 = [ "describe attributes of two sets"
                       , "use a Venn diagram to sort objects"
                       , "2D shape does not belong in set"
                       , "build bars in an object bar graph"
                       , "build bars in a pictograph"
                       , "compare data in a bar graph"
                       ]
                 pb2 = [ "make simple prediction about amounts"
                       , "tally data based on 1 attribute"
                       , "organize data about objects in bar graph"
                       , "compare on graph-\"more less equal\""
                       , "use graph data to solve simple problem"
                       ]
                 cd2 = [ "choose survey question-2 possible responses"
                       , "make prediction-opinion-based data"
                       , "tally data taken from others"
                       , "use categorical data chart-organize answers"
                       , "use symbolic representation-bar graph"
                       , "communicate conclusions-graphs"
                       ]
                 lp2 = [ "collect data to nearest inch"
                       , "order numerical data-smallest to largest"
                       , "plot data on a line plot graph"
                       , "identify measrements-determine range"
                       , "determine the median of the data"
                       , "use the median to compare"
                       , "describe the shape of the graph (mode)"
                       ]
                 pc2 = [ "predict the probability of outcome"
                       , "describe the outcome of an experiment"
                       , "describe the outcome of applying variable"
                       , "use x axis first and then y-axis on a line graph"
                       , "interpret line graph-change over time"
                       , "make a prediction-change over time"
                       , "collect data on table-change over time"
                       , "plot data-line graph-change over time"
                       , "talk/write about conclusions (comparisons)"
                       ]
                 aa2 = [ "match duplicate 2D shapes"
                       , "sort duplicate 2D shapes"
                       , "sort 2D shapes-varying sizes/orientation"
                       , "choose one attribute to sort shapes"
                       , "identify triangle and circle and square and rectangle"
                       , "locate 2D shapes in the environment"
                       ]
                 gs2 = [ "identify a line and side and angle and vertex"
                       , "draw a rectangle"
                       , "place 2D shapes to fill an area"
                       , "use attribute blocks to create a shape"
                       , "locate 3D shapes in the environment"
                       , "identify 3D shapes"
                       ]
                 dc2 = [ "match 2-D outline to faces of a 3D object"
                       , "identify congruent shapes"
                       , "predict/confirm results-moving 2D shapes"
                       , "describe a motion to prove shapes congruent"
                       , "identify symmetrical 2D shapes"
                       , "locate the line of symmetry in a figure"
                       ]
                 ap2 = [ "identify a right angle"
                       , "identify acute and obtuse angles"
                       , "identify polygons and quadrilateral subset"
                       , "identify polygons: rhombus hexagon octagon"
                       , "use a table to organize 2D shapes"
                       ]
                 ds2 = [ "identify 3D shape faces and vertices and edges"
                       , "count 3D shape faces and vertices and edges"
                       , "use a table to organize/classify 3D shapes"
                       , "count sides and vertices on a 2D cube net"
                       , "build/identify a cube from a 2D net"
                       , "sort polyhedral shapes"
                       ]
                 am2 = [ "locate holiday or birthday on calendar"
                       , "identify the seasons"
                       , "match clothing to hot/cold temperatures"
                       ]
                 ca2 = [ "name the days of the week in order"
                       , "locate the days of the week on a calendar"
                       , "name the 12 months in order"
                       , "locate date 1-10 on current calendar"
                       , "count within 10 calendar days to event"
                       , "identify season for a given month"
                       ]
                 ti2 = [ "tell analog time to the hour"
                       , "tell analog time to the 1/2 hour"
                       , "tell analog time to the 1/4 hour"
                       , "match analog/digital times on a clock"
                       , "tell time as \"quarter to\" - \"quarter after\""
                       ]
                 mt2 = [ "identify commonalities in measuring tools"
                       , "identify measurement tools given name"
                       , "match measurement attributes to tools"
                       , "match measurement tool to usage"
                       , "compare measurement attributes"
                       , "identify uses for money"
                       , "name 4 different coins"
                       , "name coin amounts for each coin named"
                       , "combine set of 4 quarters to make dollar"
                       ]
                 tm2 = [ "tell time to 5 minutes on an analog clock"
                       , "tell time: \"almost\" - 5 min to/after hour"
                       , "match coin equivalencies"
                       , "count coin combinations to $1.00"
                       , "name dollar amounts: $1 $5 $10 $20 $50"
                       , "round money amounts to next $1-2"
                       , "match sample items to general prices"
                       , "choose correct dollars to purchase item"
                       ]
                 wl2 = [ "use same objects and balance scale- make equal"
                       , "use scale and describe object weight in oz. and lbs."
                       , "identify two common weights"
                       , "identify length with lines and pictured ruler" -- Exception!
                       , "measure line length in inches (ruler)"
                       , "measure line length in feet (ruler and yardstick)"
                       , "measure line length in meters (meter stick)"
                       ]
                 mg2 = [ "measure perimeter"
                       , "measure area in units"
                       , "make different rectangles from same tiles"
                       , "determine \"cubed\" volume using cubes"
                       , "identify measuring 1 C 1/2 C 1/4 C" -- Exception!
                       , "identify liquid measuring 1 C 1/2 C 1/4 C" -- Exception!
                       , "identify T and tsp abbreviations"
                       , "measure dry ingredients with 1 C 1/2 C 1/4 C"
                       , "measure liquid with 1 C 1/2 C 1/4 C"
                       , "measure liquid and dry with 1 T and 1 tsp and 1/2 tsp"
                       ]
                 c12 = [ "recognize quantity of 2 is more than 1"
                       , "match sets of 1 and 2"
                       , "identify amounts in sets of 1 and 2"
                       , "count to 5"
                       , "identify numerals 0-5"
                       , "construct a set to match numerals 1 and 2"
                       , "write numerals 1 and 2 to match sets"
                       ]
                 c32 = [ "demonstrate 1:1 correspondence"
                       , "match sets of 3 and 4"
                       , "identify amounts in sets of 3 and 4"
                       , "construct set to match numerals 3 and 4"
                       , "write numerals 3 and 4 to match sets"
                       , "demonstrate cardinality of number"
                       ]
                 c52 = [ "count to 10"
                       , "identify numerals 6-10"
                       , "identify amounts in sets of 5 and 6"
                       , "construct set to match numerals 5 and 6"
                       , "write numerals 5 and 6 to match sets"
                       ]
                 zm2 = [ "place \"0\" given numerals/sets 0-2"
                       , "write \"0\""
                       , "identify set that is more for sets up to 10"
                       , "identify 2 sets aare equal for sets up to 10"
                       , "identify 2 sets are equal for sets up to 10"
                       , "join/separate sets and compare result"
                       ]
                 c72 = [ "identify amounts in sets of 7 and 8"
                       , "construct set to match numerals 7 and 8"
                       , "write numerals 7 and 8 to match sets"
                       , "identify amounts in sets of 9 and 10"
                       , "construct set to match numerals 9 and 10"
                       , "write numerals 9 and 10 to match sets"
                       , "identify number words one through five"
                       ]
                 on2 = [ "use ordinal numbers 1-6 to describe line"
                       , "locate numbers 1-10 on number line"
                       , "place numerals 1-10 in order"
                       , "identify relative position numerals 1-10"
                       , "compare 1-10 with &lt; and &gt; and = and language"
                       , "identify number words six through ten"
                       , "state one more than given number 1-10"
                       , "state one less than given number 1-10"
                       ]
                 ct2 = [ "identify dice patterns 1-6"
                       , "play board game using 1 die to count/move"
                       , "compose/decompose object/numeral sets 2-4"
                       , "compose/decompose object/numeral sets 5-6"
                       , "compose/decompose object/numeral sets 7-8"
                       , "compose/decompose object/numeral sets 9"
                       , "compose/decompose object/numeral sets 10"
                       ]
                 as2 = [ "solve addition problems for sums to 5"
                       , "use counting on/number line to solve (+)"
                       , "count backwards from any number 1-10"
                       , "solve ( - ) problems for corresponding sums to 5"
                       , "use counting back/number line to solve ( - )"
                       , "solve addition problems for sums 6-9"
                       , "solve ( - ) problems for corresponding sums 6-9"
                       , "solve +/- problems for (corresponding) sums 10"
                       ]
                 mm2 = [ "find missing addend to make quantity of 10"
                       , "write +/- equations horizontally/vertically"
                       , "use calculator to +/- to sums of 10"
                       , "use doubles + to solve - problems"
                       , "choose strategy to solve word problem"
                       , "count to 20"
                       ]
                 c22 = [ "identify numerals 11-15"
                       , "identify amounts in sets of 11-15"
                       , "construct a set to match numerals 11-15"
                       , "write numerals 11-15 to match sets"
                       , "identify numerals 16-20"
                       , "identify amounts in sets of 16-20"
                       , "construct a set to match numerals 16-20"
                       , "write numerals 16-20 to match sets"
                       ]
                 cn2 = [ "compare 11-20 with &lt; and &gt; and = and language"
                       , "locate numbers 11-20 on number line"
                       , "count backwards from any number 11-20"
                       , "place numerals 11-20 in order"
                       , "state one more than given number for 11-20"
                       , "state one less than given number for 11-20"
                       ]
                 n52 = [ "compare sets 1-20: greater fewer most least"
                       , "order quantities most to least/least to most"
                       , "count to 50"
                       , "skip count by tens to 100"
                       , "count/group objects in tens and ones to 100"
                       , "exchange 10 ones for 1 ten for place value to 50"
                       , "use 10s pattern to find 21-50"
                       , "identify 2-digit numbers 21-50"
                       , "write numerals 21-50"
                       ]
                 a72 = [ "identify 10 more than given number 20-50"
                       , "identify 10 less than given number 20-50"
                       , "choose method to solve ( + ) sums 11-15"
                       , "choose method to solve ( - ) sums 11-15"
                       , "choose method to solve ( + ) sums 16-20"
                       , "choose method to solve ( - ) sums 16-20"
                       ]
                 sa2 = [ "( + ) word problems to join 2 groups"
                       , "( - ) word problems - removal"
                       , "( - ) word problems - comparison"
                       , "( - ) word problems - missing part"
                       , "choose operation +/- to solve word problem"
                       , "show commutative property ( + ) equations"
                       ]
                 a32 = [ "identify/solve doubles ( + ) problems sums 11-18"
                       , "solve corresponding ( - ) problems sums of 11-18"
                       , "add 3 or more single-digit numbers"
                       , "show associative property + 1-digit numbers"
                       , "use calculator to add three 1-digit numbers"
                       ]
                 dn2 = [ "count to 100"
                       , "group objects in 10s and 1s to build 2-digit numbers"
                       , "exchange 10 ones for ten for place value 51-99"
                       , "use number patterns to find 51-100 on chart"
                       , "identify 2-digit numerals 51-99 "
                       , "write 2-digit numerals 51-99"
                       , "identify 10 more than given number 51-100"
                       , "identify 10 less than given number 51-100"
                       , "estimate reasonable 1- and 2-digit numbers for sets"
                       ]
                 dp2 = [ "exchange 10 tens for hundred; place value"
                       , "identify 3-digit numbers"
                       , "write 3-digit numbers"
                       , "+/- 10 from a 2-digit number"
                       , "+/- 100 from a 3-digit number"
                       , "use multiples of making 10 to make 100"
                       , "use calculator to +/- 2- and 3-digit numbers"
                       , "+/- 2-digit numbers without regrouping"
                       , "+/- 3-digit numbers without regrouping"
                       ]
                 re2 = [ "estimate reasonable 3-digit number for sets"
                       , "round numbers to tens place value"
                       , "round to estimate sums/differences: tens place"
                       , "round numbers to hundreds place value"
                       , "round to estimate sums/differences: hundreds place"
                       , "+/- 2-digit numbers with regrouping"
                       , "+/- 3-digit numbers with regrouping"
                       ]
                 ln2 = [ "exchange 10 hundreds for thousand; place value"
                       , "identify 4-digit numbers"
                       , "write 4-digit numbers"
                       , "estimate 4-digit number to represent familiar sets"
                       , "+/- 4-digit numerals with a calculator"
                       , "identify 5- and 6-digit numerals"
                       , "write 5- and 6-digit numbers"
                       , "use &lt; and &gt; and = to compare numbers to 6-digits"
                       ]
                 md2 = [ "use = sets to show understand multiplication"
                       , "use objects to multiply (factors 1-5 and 0)"
                       , "use 10:1 or 2:1 relationships to solve multiplication problems"
                       , "use skip counting by 5s and 2s and 10s to multiply"
                       , "multiplication (with dot sign) problems for factors 6-9"
                       ]
                 sm2 = [ "write x problem vertically/horizontally"
                       , "solve multiplication problems for factor of 10"
                       , "multiply with 10 and 100"
                       , "use multiplication to solve repeated addition problem"
                       , "solve 2-digit x problem with calculator"
                       , "demo commutative property x equations"
                       ]
                 dm2 = [ "identify number sets that can and cannot divide evenly"
                       , "use array model/grouping to demo division"
                       , "solve problems with divisors/quotients 1-5"
                       , "solve problems with divisors/quotients 6-9"
                       , "inverse ( x ) with divisors/quotients 1-9"
                       ]
                 sd2 = [ "write simple / equation using long division sign"
                       , "solve division problems with divisor of 10"
                       , "use division to solve word problem with equal sets"
                       , "divide by 100s and 10s++++"
                       , "solve problems; 2-digit divisor-calculator"
                       , "choose multiplication or division to solve word problem"
                       , "identify factors and multiples - relate to division"
                       ]
                 fw2 = [ "sort equal fraction pieces to make whole"
                       , "show 1/2 of an object and array"
                       , "assemble name equal fraction pieces-whole"
                       , "identify 2 ways to make square into fourths"
                       , "define numerator and denominator"
                       , "write fraction name to match model (1/2- 1/8)"
                       , "identify 1/2 and 1/3 and 1/4 of a set"
                       ]
                 cf2 = [ "use fraction models; numerator &gt; 1"
                       , "use fraction models to match equivalents"
                       , "identify fractions in a set; numerator &gt; 1"
                       , "order common fraction with measuring cups"
                       , "compare fractions 0 1/2 1\""
                       , "identify linear measurement 1/2\" 1/4\""
                       ]
                 af2 = [ "identify common denominator fractions"
                       , "add/subtract common denominator fractions"
                       , "add common denominator fractions to total 1 whole"
                       , "identify mixed number amount in recipe"
                       , "solve fraction addition problem for a mixed number"
                       ]
                 fd2 = [ "use fraction models to read amounts"
                       , "divide fractions 1/10 - 9/10 to decimals"
                       , "read decimals in the tenths place"
                       , "read decimals from .01 to .99 - money terms"
                       , "add/subtract decimals as money"
                       , "match decimals to fractions"
                       , "match fractions to percentages"
                       ]
