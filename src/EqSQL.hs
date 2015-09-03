module EqSQL where

import           EqCommon
import           EqLessons
import           Database.SQLite
import           Data.Maybe
import           Data.List
import qualified Data.Map        as Map
import           Data.Map           (Map)
import qualified Data.Sequence   as Seq
import           Data.Sequence      (Seq)
import qualified Data.Text       as Text
import           Data.Text          (Text)

initDB :: EqVersion -> String -> IO (Maybe String)
initDB v n = do let vSet = Map.lookup v lessonSets
                    len Nothing  = 0
                    len (Just l) = length l
                    cols  = "\"id\",\"teacher\"," ++ (intercalate "," $ (show . show) <$> [0..(len vSet)])
                    query = "CREATE TABLE " ++ show v ++ " (" ++ cols ++ ")"
                s <- openConnection n
                e <- execStatement_ s query
                closeConnection s
                return e

saveForm :: EqVersion -> Form -> IO (Maybe String)
saveForm v f = do let dbName = "EqDB"
                      tests = encodeForm f
                      len   = (length . concatMap encodeCategory $ snd f) - 1
                      cols  = intercalate "," $ map (show . show) [0..len]
                      query = "INSERT OR REPLACE INTO " ++ show v ++ " (\"id\"," ++ cols ++ ") VALUES (\"" ++ fst f ++ "\"," ++ tests ++ ")"
                  s <- openConnection dbName
                  e <- execStatement_ s query
                  closeConnection s
                  return e

delForm :: EqVersion -> String -> IO (Maybe String)
delForm v i = do let dbName = "EqDB"
                     query = "DELETE FROM " ++ show v ++ " WHERE id='" ++ i ++ "'"
                 s <- openConnection dbName
                 e <- execStatement_ s query
                 closeConnection s
                 return e

retrieveForm :: EqVersion -> String -> IO (Either String [[Row String]])
retrieveForm v i = do let dbName = "EqDB"
                          query = "SELECT * FROM " ++ show v ++ " WHERE id='" ++ i ++ "'"
                      s <- openReadonlyConnection dbName
                      e <- execStatement s query
                      closeConnection s
                      return e
