{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Events
  (appEvent
  , handleMainListEvent
  , handleWatchListEvent
  , handleNameEditLEvent
  , startEvent
  ) where

import Control.Lens
import Control.Monad.Trans

import Data.Text (pack, unpack)

import Brick.Main
import Brick.Types
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Dialog
import Graphics.Vty

import Database.SQLite.Simple (withConnection)

import Types
import DBTools
import GameTools

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
  case st^.screen of
    MainList   -> handleMainListEvent st e
    WatchList  -> handleWatchListEvent st e
    WatchUserList -> handleWatchUserListEvent st e
    NameEditorL -> handleNameEditLEvent st e
    PassEditorL -> handlePassEditLEvent st e
    NameEditorR -> handleNameEditREvent st e
    PassEditorR -> handlePassEditREvent st e
    EmailEditor -> handleEmailEditEvent st e
    PassEditorC -> handlePassEditCEvent st e
    ConfDialogR -> handleConfDialogREvent st e
    UserMenu   -> handleUserMenuEvent st e
    GameMenu   -> handleGameMenuEvent st e

handleMainListEvent :: St -> Event -> EventM (Next St)
handleMainListEvent st e =
  case e of
    --EvKey KEnter [] -> continue $ st { _screen = WatchList }
    EvKey KEnter [] -> case listSelection st _mainList of
                         "login"  ->  toScreen st NameEditorL
                         "register"-> toScreen st NameEditorR
                         "watch"  -> do
                                     newSt <- populateWatchList st
                                     toScreen newSt WatchList
                         "logout" ->  halt st
                         _ -> halt st
    EvKey KUp []   -> passToWidget st e mainList
    EvKey KDown [] -> passToWidget st e mainList
    EvKey KEsc []  -> halt st
    _ -> continue st
   -- implement error logging

populateWatchList st = do
                  gameNames <- liftIO $ getGameNames
                  let newList = listReplace gameNames (_watchList st)
                  let newSt = st {_watchList=newList}
                  return newSt

handleWatchListEvent :: St -> Event -> EventM (Next St)
handleWatchListEvent st e =
  case e of
    EvKey KEnter [] -> do
                       sessions <- liftIO $ getSessions (listSelection st _watchList)
                       let newList = listReplace sessions (_watchUserList st)
                       let newSt = st {_watchUserList=newList}
                       toScreen newSt WatchUserList
    EvKey KUp []    -> passToWidget st e watchList
    EvKey KDown []  -> passToWidget st e watchList
    EvKey KEsc []   -> toScreen st destination
    _ -> toScreen st destination
  where destination = if _authenticated st
                         then UserMenu
                         else MainList

handleWatchUserListEvent :: St -> Event -> EventM (Next St)
handleWatchUserListEvent st e =
  case e of
    EvKey KEnter [] -> suspendAndResume $ viewGame (listSelection st _watchUserList) st
    EvKey KUp []    -> passToWidget st e watchUserList
    EvKey KDown []  -> passToWidget st e watchUserList
    EvKey KEsc []   -> toScreen st WatchList
    _ -> toScreen st WatchList

handleNameEditLEvent :: St -> Event -> EventM (Next St)
handleNameEditLEvent st e =
  case e of
    EvKey KEnter [] -> continue $ st { _screen = PassEditorL}
    EvKey KEsc []   -> continue $ st { _screen = MainList}
    _ -> passToWidget st e nameEditorL

handlePassEditLEvent :: St -> Event -> EventM (Next St)
handlePassEditLEvent st e =
  case e of
    EvKey KEnter [] -> do
                       result <- liftIO $ withConnection dbPath (attemptLogin name pass)
                       if result >= 0
                          then continue $ st { _screen = UserMenu, _userID = result
                                             , _authenticated = True, _userName = unpack name
                                             , _passWord = unpack pass}
                          else continue $ st { _screen = MainList}
    EvKey KEsc []   -> continue $ st { _screen = MainList}
    _ -> passToWidget st e passEditorL
  where name = pack $ head $ getEditContents $ _nameEditorL st -- don't use head. doh
        pass = pack $ head $ getEditContents $ _passEditorL st

handleNameEditREvent :: St -> Event -> EventM (Next St)
handleNameEditREvent st e =
  case e of
    EvKey KEnter [] -> continue $ st { _screen = PassEditorR}
    EvKey KEsc []   -> continue $ st { _screen = MainList}
    _ -> passToWidget st e nameEditorR
{-
handlePassEditREvent :: St -> Event -> EventM (Next St)
handlePassEditREvent st e =
  case e of
    EvKey KEnter [] -> do
                       let conn = st^.connection
                       result <- liftIO $ attemptRegister conn name pass email
                       if result > 0
                          then continue $ st { _screen = UserMenu, _userID=result, _authenticated=True}
                          else continue $ st { _screen = MainList}
    EvKey KEsc []   -> continue $ st { _screen = MainList}
    _ -> passToWidget st e passEditorR
  where name = pack $ head $ getEditContents $ _nameEditorR st -- don't use head. doh
        pass = pack $ head $ getEditContents $ _passEditorR st
-}

handlePassEditREvent :: St -> Event -> EventM (Next St)
handlePassEditREvent st e =
  case e of
    EvKey KEnter [] -> toScreen st EmailEditor
    EvKey KEsc [] -> toScreen st MainList
    _ -> passToWidget st e passEditorR

handleEmailEditEvent :: St -> Event -> EventM (Next St)
handleEmailEditEvent st e =
  case e of
    EvKey KEnter [] -> continue $ st { _screen = ConfDialogR }
    EvKey KEsc []   -> continue $ st { _screen = MainList }
    _ -> passToWidget st e emailEditor

handlePassEditCEvent :: St -> Event -> EventM (Next St)
handlePassEditCEvent st e =
  case e of
    EvKey KEnter [] -> do
                       liftIO $ withConnection dbPath $ updateUserPass (_userID st) (head $getEditContents $ _passEditorC st)
                       toScreen st UserMenu
    EvKey KEsc [] -> toScreen st UserMenu
    _ -> passToWidget st e passEditorC

handleConfDialogREvent :: St -> Event -> EventM (Next St)
handleConfDialogREvent st e =
  case e of
    EvKey KEnter [] -> case dialogSelection (_confDialogR st ) of
                         Just True -> do
                           result <- liftIO $ withConnection dbPath (attemptRegister name pass mail)
                           if result >= 0 -- TODO: don't do this... pls
                              then continue $ st { _screen = UserMenu, _userID=result, _authenticated=True
                                                 , _userName = unpack name, _passWord = unpack pass}
                              else toScreen st MainList
                         Just False -> toScreen st MainList
                         _ -> toScreen st MainList
    EvKey KEsc [] -> toScreen st MainList
    _ -> passToWidget st e confDialogR
  where name = pack $ head $ getEditContents $ _nameEditorR st -- don't use head. doh
        pass = pack $ head $ getEditContents $ _passEditorR st
        mail = pack $ head $ getEditContents $ _emailEditor st

handleUserMenuEvent :: St -> Event -> EventM (Next St)
handleUserMenuEvent st e =
  case e of
    EvKey KEnter [] -> case listSelection st _userMenu of
                         "play game" -> do
                           gameNames <- liftIO $ getGameNames
                           let newList = listReplace gameNames (_gameMenu st)
                           let newSt = st {_gameMenu=newList}
                           toScreen newSt GameMenu
                         "watch" -> do
                                    newSt <- populateWatchList st
                                    toScreen newSt WatchList
                         "change pw" -> toScreen st PassEditorC
                         "logout" -> halt st
                         _ -> halt st
    EvKey KUp []   -> passToWidget st e userMenu
    EvKey KDown [] -> passToWidget st e userMenu
    _ -> continue st

handleGameMenuEvent :: St -> Event -> EventM (Next St)
handleGameMenuEvent st e =
  case e of
    EvKey KEnter [] -> suspendAndResume $ startGame (listSelection st _gameMenu) st
    EvKey KUp []  -> passToWidget st e gameMenu
    EvKey KDown [] -> passToWidget st e gameMenu
    EvKey KEsc [] -> toScreen st UserMenu
    _ -> continue st
--handlePassEditEvent :: St -> Event -> EventM (Next St)
-- those are going to be very similar. combine them into one function handleEditEvent

startGame :: String -> St -> IO St
startGame sel st = do
   games <- getGames
   launchGame sel (pack(st^.userName)) games
   return st
          
viewGame :: String -> St -> IO St
viewGame sel st = do
   watchGame sel
   return st

startEvent :: St -> EventM St
startEvent = return

listSelection :: St -> (St -> List String) -> String
listSelection st l = case (listSelectedElement $ l st) of
                     Just (_, x) -> x
                     Nothing -> "error"

passToWidget :: HandleEvent b =>
     a -> Event -> Setting (->) a r b b -> EventM (Next r)
passToWidget st e w = continue $ st & w %~ handleEvent e

toScreen :: St -> Screen -> EventM (Next St)
toScreen st s = continue $ st { _screen = s}

