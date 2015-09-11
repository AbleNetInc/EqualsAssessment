module EqSQL where

import           EqCommon
import           EqLessons
import           Database.SQLite
import           Data.Maybe
import           Data.List
import           Data.Foldable      (toList)
import qualified Data.Map        as Map
import           Data.Map           (Map)
import qualified Data.Sequence   as Seq
import           Data.Sequence      (Seq)
import qualified Data.Text       as Text
import           Data.Text          (Text)

initDB :: String -> EqVersion -> IO (Maybe String)
initDB d v = do let vSet = Map.lookup v lessonSets
                    len Nothing  = 0
                    len (Just l) = length l
                    cols  = intercalate "," $ show <$> ("id" : "teacher" : (show <$> [0..(len vSet)-1]))
                    query = concat ["create table ",show v," (",cols,");"]
                h <- openConnection d
                e <- execStatement_ h query
                closeConnection h
                return e

saveAssessment :: String -> Assessment -> IO (Maybe String)
saveAssessment d (Assessment s v t l)
             = do deleteAssessment v d (Text.unpack s) $ Text.unpack t
                  let cols = "\"id\"" : "\"teacher\"" : ((show . show) <$> [0..(Seq.length l)-1])
                      vals = (Text.unpack <$> [s,t]) ++ (toList $ show <$> l)
                      row  = zip cols vals
                  h <- openConnection d
                  e <- insertRow h (show v) row
                  closeConnection h
                  return e

deleteAssessment :: EqVersion -> String -> String -> String -> IO (Maybe String)
deleteAssessment v d s t = do let query = concat ["delete from ",show v," where id=",s," and teacher=\"",t,"\";"]
                              h <- openConnection d
                              e <- execStatement_ h query
                              closeConnection h
                              return e

rowToAssessment :: EqVersion -> (Row String) -> Assessment
rowToAssessment v r = Assessment id v t $ Seq.fromList ls
                    where id = Text.pack . fromJust $ lookup "id"      r
                          t  = Text.pack . fromJust $ lookup "teacher" r
                          vS = Map.lookup v lessonSets
                          l Nothing     = 0
                          l (Just lset) = Seq.length lset
                          ls = [(read . fromJust $ lookup x r) :: Lesson| x <- show <$> [0..((l vS)-1)]]

retrieveAssessment :: String -> EqVersion -> String -> String -> IO Assessment
retrieveAssessment d v s t = do let query = concat ["select * from ",show v," where id=",s," and teacher=\"",t,"\";"]
                                h <- openReadonlyConnection d
                                e <- execStatement h query
                                closeConnection h
                                case e of
                                     Right ([]:_) -> return $ blankAssessment v s t
                                     Right (r:_)  -> return . rowToAssessment v $ head r
                                     Left s       -> error s
