{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types
  ( St(..)
  , Screen(..)
  , User(..)
--  , Event(..)
  , initialState
  , screen, mainList, watchList, watchUserList, nameEditorL, passEditorL, nameEditorR, passEditorR, emailEditor, confDialogR
  , passEditorC
  , userMenu, gameMenu
  , username, password, userID, userName, passWord, userPrivs
  , idNum
  ) where
  
import Data.Text (Text)
import Control.Lens
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Dialog

import Database.SQLite.Simple (FromRow, fromRow, field)

data User = User { _idNum :: Int
                 , _username :: Text
                 , _password :: Text
                 , _email :: Text
                 , _admin :: Bool
                 , _mod :: Bool
                 } deriving (Show)

makeLenses ''User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

data Screen = MainList
            | WatchList
            | WatchUserList
            | NameEditorL
            | PassEditorL
            | NameEditorR
            | PassEditorR
            | EmailEditor
            | PassEditorC
            | ConfDialogR
            | UserMenu
            | GameMenu
            deriving (Show, Eq)

-- TODO: add error logging to St
data St = St { _screen :: Screen
             , _userID :: Int
             , _userName :: String
             , _passWord :: String
             , _userPrivs :: (Bool,Bool)
             , _authenticated :: Bool
             , _mainList :: List String
             , _watchList :: List String
             , _watchUserList :: List String
             , _nameEditorL :: Editor
             , _passEditorL :: Editor
             , _nameEditorR :: Editor
             , _passEditorR :: Editor
             , _passEditorC :: Editor
             , _emailEditor :: Editor
             , _confDialogR :: Dialog Bool
             , _userMenu :: List String
             , _gameMenu :: List String
             }

makeLenses ''St
             
--TODO: add user information
--      not sure if (userName, userPass, ...) or User {userName="a",userPass="b", ...}
--      probably User data type
initialState :: St
initialState =
  St { _screen = MainList
     , _userID = (-1)
     , _userName = ""
     , _passWord = ""
     , _userPrivs = (False,False)
     , _authenticated = False
     , _mainList = list (Name "mainList") (const str) ["login", "register", "watch", "logout"]
     , _watchList = list (Name "watchList") (const str) ["a","b","c"]
     , _watchUserList = list (Name "watchUserList") (const str) ["a","b","c"]
     , _nameEditorL = editor (Name "nameEditorL") (str.unlines) (Just 1) ""
     , _passEditorL = editor (Name "passEditorL") (str.unlines) (Just 1) ""
     , _nameEditorR = editor (Name "nameEditorR") (str.unlines) (Just 1) ""
     , _passEditorR = editor (Name "passEditorR") (str.unlines) (Just 1) ""
     , _emailEditor = editor (Name "emailEditor") (str.unlines) (Just 1) ""
     , _passEditorC = editor (Name "passEditorC") (str.unlines) (Just 1) ""
     , _confDialogR = dialog (Name "confDialogR") (Just "hello") (Just (0, [("Yes", True),("No", False)])) 40
     , _userMenu = list (Name "userMenu") (const str) ["play game", "watch", "change pw", "logout"]
     , _gameMenu = list (Name "gameMenu") (const str) []
     }

