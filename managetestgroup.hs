{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import ResultsDB(getConnectionFromTrunk)
import Database.HDBC(toSql,fromSql,withTransaction,prepare,execute,fetchRow)
import Database.HDBC.Statement
import Database.HDBC.Sqlite3(Connection)
import System.Environment(getArgs)
import Data.Maybe
import Control.Monad(void, join, liftM2)
import System.Exit

stmtMakeGroup :: String
stmtMakeGroup = "INSERT INTO test_groups (description) VALUES (?)"

stmtAddFileToGroup :: String
stmtAddFileToGroup = "INSERT INTO test_group_memberships (group_id,test_id) VALUES (?,?)"

stmtUpdateDesc :: String
stmtUpdateDesc = "UPDATE test_groups SET description=? WHERE id=?"

stmtGetGroupId :: String
stmtGetGroupId = "SELECT id FROM test_groups WHERE description=?"

stmtGetLatestGroup :: String
stmtGetLatestGroup = "SELECT id from test_groups ORDER BY id DESC LIMIT 1"

-- Returns the ID of the group we created
makeGroup :: String -> Connection -> IO Int
makeGroup desc connection =
  withTransaction connection (
    \con -> do
      mkstmt <- prepare con stmtMakeGroup
      execute mkstmt [toSql desc]
      getstmt <- prepare con stmtGetLatestGroup
      execute getstmt []
      fmap (fromSql.head.fromJust) $ fetchRow getstmt)

addFileToGroup :: Connection -> Int -> Statement -> String -> IO ()
addFileToGroup con gid stmt tid = void $ execute stmt [toSql gid,toSql tid]

updateDesc :: Int -> String -> Connection -> IO ()
updateDesc gid desc con = do
  stmt <- prepare con stmtUpdateDesc
  void $ execute stmt [toSql desc, toSql gid]

getGroupId :: String -> Connection -> IO Int
getGroupId desc con = do
  stmt <- prepare con stmtGetGroupId
  execute stmt [toSql desc]
  fmap (fromSql.head.fromJust) $ fetchRow stmt

addFiles :: Connection -> Int -> [String] -> IO ()
addFiles con gid filenames = do
  stmt <- prepare con stmtAddFileToGroup
  void $ mapM (addFileToGroup con gid stmt) filenames

createGroup :: [String] -> Connection -> IO ()
createGroup (desc:files) con = do
  gid <- makeGroup desc con
  addFiles con gid files
createGroup _ _ = usage $ ExitFailure 2

appendByDesc :: [String] -> Connection -> IO ()
appendByDesc (desc:files) con = do
  gid <- getGroupId desc con
  addFiles con gid files
appendByDesc _ _ = usage $ ExitFailure 2

appendById :: [String] -> Connection -> IO ()
appendById (gid:files) con = do
  addFiles con (read gid) files
appendById _ _ = usage $ ExitFailure 2

amendDesc :: [String] -> Connection -> IO ()
amendDesc (gid:desc:[]) con = do
  updateDesc (read gid) desc con
amendDesc _ _ = usage $ ExitFailure 2

usage :: ExitCode -> IO ()
usage exitCode = do
  putStrLn $ unlines [
    "managetestgroup [COMMAND] ... [OPTIONS]",
    "",
    "managetestgroup help",
    "  Show this help message",
    "",
    "managetestgroup creategroup DESC [FILES]",
    "  Create a test group with description DESC, containing FILES",
    "",
    "managetestgroup appendbydesc DESC [FILES]",
    "  Add FILES to the test group with description DESC",
    "",
    "managetestgroup appendbyid ID [FILES]",
    "  Add FILES to the test group with numeric identifier ID",
    "",
    "managetestgroup amenddesc ID NEWDESC",
    "  Replace the description of group number ID with NEWDESC" ]
  exitWith exitCode

dispatch :: Connection -> [String] -> IO ()
dispatch _   ("--help":_)          = usage ExitSuccess
dispatch _   ("help":_)            = usage ExitSuccess
dispatch _   ("-h":_)              = usage ExitSuccess
dispatch con ("creategroup":args)  = withTransaction con $ createGroup args
dispatch con ("appendbydesc":args) = withTransaction con $ appendByDesc args
dispatch con ("appendbyid":args)   = withTransaction con $ appendById args
dispatch con ("amenddesc":args)    = withTransaction con $ amendDesc args
dispatch _   _                     = usage $ ExitFailure 2

main :: IO ()
main = join $ (liftM2 dispatch) getConnectionFromTrunk getArgs
