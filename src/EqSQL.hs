module EqSQL where

import EqCommon
import EqLessons
import Database.SQLite
import Data.Maybe
import Data.List

saveForm :: EqVersion -> Form -> IO (Maybe String)
saveForm v f = do let dbName = "EqDB"
                  s <- openConnection dbName
                  let tests = encodeForm f
                  let cols  = intercalate "," $ map (show . show) [0..((length . concatMap (encodeCategory) $ snd f)-1)]
                  let query = "INSERT OR REPLACE INTO " ++ show v ++ " (\"id\"," ++ cols ++ ") VALUES (\"" ++ fst f ++ "\"," ++ tests ++ ")"
                  e <- execStatement_ s query
                  closeConnection s
                  return e

delForm :: EqVersion -> String -> IO (Maybe String)
delForm v i = do let dbName = "EqDB"
                 s <- openConnection dbName
                 let query = "DELETE FROM " ++ show v ++ " WHERE id='" ++ i ++ "'"
                 e <- execStatement_ s query
                 closeConnection s
                 return e

retrieveForm :: EqVersion -> String -> IO (Either String [[Row String]])
retrieveForm v i = do let dbName = "EqDB"
                      s <- openReadonlyConnection dbName
                      let query = "SELECT * FROM " ++ show v ++ " WHERE id='" ++ i ++ "'"
                      e <- execStatement s query
                      closeConnection s
                      return e
