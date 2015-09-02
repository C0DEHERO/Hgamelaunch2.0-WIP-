{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Types
  ( St(..)
  , Screen(..)
  , User(..)
  , Game(..)
--  , Event(..)
  , initialState
  , screen, mainMenu, watchUsers, nameEditor, passEditor, emailEditor, confDialog
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

import Control.Applicative

import Data.Aeson
import GHC.Generics

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

data Screen = MainMenu
            | WatchGames
            | WatchUsers
            | NameEditorL
            | PassEditorL
            | NameEditorR
            | PassEditorR
            | EmailEditor
            | PassEditorC
            | ConfDialog
            | UserMenu
            | GameMenu
            deriving (Show, Eq)

data Game = Game
      { shortname   :: Text
      , gamename    :: Text
      , rootpath    :: Text
      , gamepath    :: Text
      , userdir     :: Text
      , sessiondir  :: Text
      , templatecfg :: Text
      , cfgfile     :: Text
      , gameargs    :: [Text]
      } deriving (Show, Read,Generic)

instance FromJSON Game
instance ToJSON Game


-- TODO: add error logging to St
data St = St { _screen :: Screen
             , _userID :: Int
             , _userName :: String
             , _passWord :: String
             , _emailAddr :: String
             , _userPrivs :: (Bool,Bool)
             , _authenticated :: Bool
             , _mainMenu :: List String
             , _watchUsers :: List String
             , _nameEditor :: Editor
             , _passEditor :: Editor
             , _passEditorC :: Editor
             , _emailEditor :: Editor
             , _confDialog :: Dialog Bool
             , _userMenu :: List String
             , _gameMenu :: List String
             }

makeLenses ''St

--TODO: add user information
--      not sure if (userName, userPass, ...) or User {userName="a",userPass="b", ...}
--      probably User data type
initialState :: St
initialState =
  St { _screen = MainMenu
     , _userID = (-1)
     , _userName = ""
     , _passWord = ""
     , _emailAddr = ""
     , _userPrivs = (False,False)
     , _authenticated = False
     , _mainMenu = list (Name "mainMenu") (const str) ["login", "register", "watch", "logout"]
     , _watchUsers = list (Name "watchUsers") (const str) []
     , _nameEditor = editor (Name "nameEditor") (str.unlines) (Just 1) ""
     , _passEditor = editor (Name "passEditor") (str.hidePw) (Just 1) ""
     , _emailEditor = editor (Name "emailEditor") (str.unlines) (Just 1) ""
     , _passEditorC = editor (Name "passEditorC") (str.unlines) (Just 1) ""
     , _confDialog = dialog (Name "confDialog") (Just "Is this right?") (Just (0, [("Yes", True),("No", False)])) 40
     , _userMenu = list (Name "userMenu") (const str) ["play game", "watch", "change pw", "logout"]
     , _gameMenu = list (Name "gameMenu") (const str) []
     }
  where hidePw = init . map (const '*') . unlines
