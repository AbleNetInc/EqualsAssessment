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

initDB :: EqVersion -> String -> IO (Maybe String)
initDB v d = do let vSet = Map.lookup v lessonSets
                    len Nothing  = 0
                    len (Just l) = length l
                    cols  = intercalate "," $ show <$> ("id" : "teacher" : (show <$> [0..(len vSet)-1]))
                    query = concat ["create table ",show v," (",cols,");"]
                h <- openConnection d
                e <- execStatement_ h query
                closeConnection h
                return e

-- Needs to make sure we only overwrite where the id and the teacher match
saveAssessment :: String -> Assessment -> IO (Maybe String)
saveAssessment d (Assessment s v t l)
             = do let cols = "id" : "teacher" : ((show . show) <$> [0..(Seq.length l)-1])
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

retrieveAssessment :: String -> EqVersion -> String -> String -> IO (Either String [[Row String]])
retrieveAssessment d v s t = do let query = concat ["select * from ",show v," where id=",s," and teacher=\"",t,"\";"]
                                h <- openReadonlyConnection d
                                e <- execStatement h query
                                closeConnection h
                                return e
