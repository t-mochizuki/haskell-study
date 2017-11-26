{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Entity.User where

import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (connectSqlite3)

defineTableFromDB
  (connectSqlite3 "weight.db")
  (driverSQLite3 { typeMap = [("INTEGER", [t|Int|])] })
  "main"
  "user"
  [''Show]
