{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import ResultsDB(getConnectionFromTrunk)
import Database.HDBC(toSql,fromSql,withTransaction,prepare,execute,fetchRow)
import Database.HDBC.Sqlite3(Connection)
import System.Console.CmdArgs
import Data.Maybe
import Control.Monad(void)

data ManageTestGroup = CreateGroup
                       { groupDescription :: String
                       , files :: [String]
                       } |
                       AppendByDesc
                       { groupDescription :: String
                       , files :: [String]
                       } |
                       AppendById
                       { groupId :: Int
                       , files :: [String]
                       } |
                       AmendDesc
                       { groupId :: Int
                       , groupDescription :: String
                       } deriving (Data,Typeable,Show)

createDefaults :: ManageTestGroup
createDefaults = CreateGroup
             { groupDescription  = "" &= help "A description of this test group"
             , files = [] &= args
             }

appendByDescDefaults :: ManageTestGroup
appendByDescDefaults = AppendByDesc
                       { groupDescription = "" &= help "The description of the group to update"
                       , files = [] &= args
                       }
appendByIdDefaults :: ManageTestGroup
appendByIdDefaults = AppendById
                       { groupId = 0 &= help "The id of the group to update"
                       , files = [] &= args
                       }
amendDescDefaults :: ManageTestGroup
amendDescDefaults = AmendDesc
                    { groupId = 0 &= help "The id of the group to amend"
                    , groupDescription = "" &= help "The new description"
                    }

stmtMakeGroup :: String
stmtMakeGroup = "INSERT INTO test_groups (description) VALUES (?)"

stmtAddFileToGroup :: String
stmtAddFileToGroup = "INSERT INTO test_group_memberships (group_id,test_id) VALUES (?,?)"

stmtUpdateDesc :: String
stmtUpdateDesc = "UPDATE test_groups SET description=? WHERE id=?"

stmtGetGroupId :: String
stmtGetGroupId = "SELECT id FROM test_groups WHERE description=?"

stmtGetLatestGroup :: String
stmtGetLatestGroup = "SELECT id from test_groups ORDER BY id DESC"

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

addFileToGroup :: Connection -> Int -> String -> IO ()
addFileToGroup con gid tid = do
  stmt <- prepare con stmtAddFileToGroup
  void $ execute stmt [toSql gid,toSql tid]

updateDesc :: Int -> String -> Connection -> IO ()
updateDesc gid desc con = do
  stmt <- prepare con stmtUpdateDesc
  void $ execute stmt [toSql desc, toSql gid]

getGroupId :: String -> Connection -> IO Int
getGroupId desc con = do
  stmt <- prepare con stmtGetGroupId
  execute stmt [toSql desc]
  fmap (fromSql.head.fromJust) $ fetchRow stmt

dispatch :: ManageTestGroup -> Connection -> IO ()
dispatch (CreateGroup desc filenames) con = do
  putStrLn $ "Creating group '" ++ desc ++ "'."
  gid <- makeGroup desc con
  putStrLn ("Created group '" ++ desc ++ "'.")
  putStrLn $ "Adding " ++ (show $ length filenames) ++ " files."
  void $ mapM (addFileToGroup con gid) filenames
  putStrLn "Added files"
dispatch (AppendByDesc desc filenames) con = do
  gid <- getGroupId desc con
  void $ mapM (addFileToGroup con gid) filenames
  error ".."
dispatch (AppendById gid filenames) con = do
  void $ mapM (addFileToGroup con gid) filenames
  error ".."
dispatch (AmendDesc gid desc) con = updateDesc gid desc con

main :: IO ()
main = do
  opts <- cmdArgs (modes [createDefaults,appendByDescDefaults, appendByIdDefaults,amendDescDefaults])
  putStrLn "Parsed arguments."
  con <- getConnectionFromTrunk
  putStrLn "Got connection."
  withTransaction con $ dispatch opts

