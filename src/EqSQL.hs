module EqSQL where

import EqCommon
import EqLessons
import Database.SQLite

saveForm :: EqVersion -> Form -> IO (Maybe String)
saveForm v f = do let dbName = "EqDB"
                  s <- openConnection dbName
                  let query = "INSERT INTO " ++ show v ++ " VALUES (\"" ++ (fst f) ++ "\",\"" ++ toCSV f ++ "\")"
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
