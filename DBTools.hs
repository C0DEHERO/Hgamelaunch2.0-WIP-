{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module DBTools 
  ( makeDb
  , dbPath
  , insertUser
  , fetchUserWithID
  , updateUserPass
  , fetchUserFromDb
  ) where

import Database.SQLite.Simple
import Data.Text
import System.Directory (copyFile, createDirectoryIfMissing)
import Filesystem.Path.CurrentOS (encodeString, decodeString, parent)

import Types

dbPath :: FilePath
dbPath = "./users.db"

makeDb :: Connection -> Bool -> IO ()
makeDb conn False = do
  execute conn "CREATE TABLE users(id INTEGER PRIMARY KEY,username VARCHAR NOT NULL,password VARCHAR NOT NULL,email VARCHAR NOT NULL,admin BOOLEAN NOT NULL,debugger BOOLEAN NOT NULL,CONSTRAINT username_key UNIQUE (username))" ()
  insertUser conn (User 0 "admin" "admin" "admin@server.org" True False)
makeDb _ True = return ()

insertUser :: Connection -> User -> IO ()
insertUser conn (User _ username password email admin debugger) = do
  execute conn "INSERT INTO users (username, password, email, admin, debugger) VALUES (?, ?, ?, ?, ?)"
               (username, password, email, admin :: Bool, debugger :: Bool)

updateUserPass :: Int -> String -> Connection -> IO ()
updateUserPass id pass conn = do
  execute conn "UPDATE users SET password=? WHERE id=?"
               ((pack pass), id :: Int)

fetchUserWithID :: Int -> Connection -> IO (Maybe User)
fetchUserWithID id conn = do
  xs <- query conn "SELECT * FROM users WHERE id = ?" (Only id)
  case xs of [] -> return Nothing
             [x] -> return (Just x)
             _ -> return Nothing

fetchUserFromDb :: Connection -> Text -> IO (Maybe User)
fetchUserFromDb conn name = do
  xs <- query conn "SELECT * FROM users WHERE username like ? LIMIT 1" (Only name)
  case xs of [] -> return Nothing
             [x] -> return (Just x)
             _ -> return Nothing
