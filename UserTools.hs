{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module UserTools
       ( attemptLogin
       , attemptRegister
       ) where

import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import Filesystem.Path.CurrentOS (encodeString, decodeString, parent)
import Control.Lens
import Data.Text
import Database.SQLite.Simple

import DBTools
import Types
import GameTools

attemptLogin :: Text -> Text -> Connection -> IO Int -- perhaps change to Either Int for error messages
attemptLogin name pass conn = do
  result <- fetchUserFromDb conn name
  return $ case result of
             Just uFromDb -> if name == (uFromDb^.username) &&
                                pass == (uFromDb^.password)
                                then uFromDb^.idNum
                                else (-1)
             Nothing -> (-1)

attemptRegister :: Text -> Text -> Text -> Connection -> IO Int
attemptRegister name pass mail conn = do
  result <- fetchUserFromDb conn name :: IO (Maybe User)
  case result of
    Nothing -> do
      let user = (User 0 name pass mail False False)
      games <- getGames
      createUserDirs name games
      insertUser conn user
      newUser <- fetchUserFromDb conn name
      case newUser of
        Just u -> return $ _idNum u
        Nothing -> return (-1)
    (Just _) -> return (-1)

createUserDirs :: Text -> [Game] -> IO()
createUserDirs username ((Game {..}):games) = do
  createDirectoryIfMissing True (substitute userdir)
  createDirectoryIfMissing True (substitute sessiondir)
  createDirectoryIfMissing True (encodeString $ parent $ decodeString $ substitute cfgfile)
  templateExists <- doesFileExist (substitute templatecfg)
  cpTemplate (substitute templatecfg) (substitute cfgfile) templateExists
  createUserDirs username games
    where substitute old = unpack $ replace "%n" username . replace "%r" rootpath . replace "%u" userdir $ old
          cpTemplate old new True = copyFile old new
          cpTemplate _ _ False = return ()
createUserDirs _ [] = return ()

