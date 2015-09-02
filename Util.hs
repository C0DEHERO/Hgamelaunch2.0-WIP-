module Util
       (logmein, registerme, changepw, listSelection, passToWidget,
        toScreen, getEditInput, getEditInputTxt, populateGameMenu)
       where

import Data.Text (Text, pack)
import Control.Lens
import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Edit
import Graphics.Vty

import Control.Monad.Trans
import Database.SQLite.Simple (withConnection)

import Types
import DBTools
import GameTools
import UserTools

logmein :: St -> EventM (Next St)
logmein st@(St{_userName=name, _passWord=pass}) = do
  result <- liftIO $ withConnection dbPath $ attemptLogin (pack name) (pack pass)
  if result >= 0
     then continue $ st { _screen = UserMenu, _userID = result
                        , _authenticated = True}
     else continue $ st { _screen = MainMenu}

registerme :: St -> EventM (Next St)
registerme st@(St{_userName = name, _passWord = pass, _emailAddr = mail}) = do
  result <- liftIO $ withConnection dbPath $ attemptRegister (pack name) (pack pass) (pack mail) -- TODO: extra function in Util.hs
  if result >= 0 -- TODO: don't do this... pls
     then continue $ st { _screen = UserMenu, _userID=result, _authenticated=True }
     else toScreen st MainMenu

changepw :: St -> EventM (Next St)
changepw st@(St{_passWord = pass, _userID = id}) = do
  liftIO $ withConnection dbPath $ updateUserPass id pass
  continue $ st { _screen = UserMenu, _passWord = pass}

listSelection :: St -> (St -> List String) -> String
listSelection st l = case (listSelectedElement $ l st) of
                     Just (_, x) -> x
                     Nothing -> "error"

passToWidget :: HandleEvent b =>
     a -> Event -> Setting (->) a r b b -> EventM (Next r)
passToWidget st e w = continue $ st & w %~ handleEvent e

toScreen :: St -> Screen -> EventM (Next St)
toScreen st s = continue $ st { _screen = s}

getEditInput :: (St -> Editor) -> St -> String
getEditInput e st = head $ getEditContents $ e st

getEditInputTxt :: (St -> Editor) -> St -> Text
getEditInputTxt e st = pack $ getEditInput e st

populateGameMenu st = do
  gameNames <- liftIO $ getGameNames
  let newList = listReplace gameNames (_gameMenu st)
  let newSt = st {_gameMenu=newList}
  return newSt

