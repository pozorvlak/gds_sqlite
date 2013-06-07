module ResultsDB (getConnectionFromTrunk) where

import Database.HDBC.Sqlite3(Connection, connectSqlite3)

getConnectionFromTrunk :: IO Connection
getConnectionFromTrunk = connectSqlite3 "gds.sqlite3"

