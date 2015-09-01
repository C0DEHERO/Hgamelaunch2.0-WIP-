{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Events
  (appEvent
  , handleMainMenuEvent
  , startEvent
  ) where

import Control.Lens
import Control.Monad.Trans

import Data.Text (Text, pack, unpack)

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
import Util

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
  case st^.screen of
    MainMenu   -> handleMainMenuEvent st e
    WatchMenu  -> handleWatchMenuEvent st e
    WatchUserList -> handleWatchUserListEvent st e
    NameEditorL -> handleNameEditEvent st e
    PassEditorL -> handlePassEditEvent st e
    NameEditorR -> handleNameEditEvent st e
    PassEditorR -> handlePassEditEvent st e
    EmailEditor -> handleEmailEditEvent st e
    PassEditorC -> handlePassEditEvent st e
    ConfDialog -> handleConfDialogEvent st e
    UserMenu   -> handleUserMenuEvent st e
    GameMenu   -> handleGameMenuEvent st e

handleMainMenuEvent :: St -> Event -> EventM (Next St)
handleMainMenuEvent st e =
  case e of
    --EvKey KEnter [] -> continue $ st { _screen = WatchMenu }
    EvKey KEnter [] -> case listSelection st _mainMenu of
                         "login"  ->  toScreen st NameEditorL
                         "register"-> toScreen st NameEditorR
                         "watch"  -> do
                                     newSt <- populateWatchMenu st
                                     toScreen newSt WatchMenu
                         "logout" ->  halt st
                         _ -> halt st
    EvKey KUp []   -> passToWidget st e mainMenu
    EvKey KDown [] -> passToWidget st e mainMenu
    EvKey KEsc []  -> halt st
    _ -> continue st
   -- implement error logging

populateWatchMenu st = do
                  gameNames <- liftIO $ getGameNames
                  let newList = listReplace gameNames (_watchMenu st)
                  let newSt = st {_watchMenu=newList}
                  return newSt

handleWatchMenuEvent :: St -> Event -> EventM (Next St)
handleWatchMenuEvent st e =
  case e of
    EvKey KEnter [] -> do
                       sessions <- liftIO $ getSessions (listSelection st _watchMenu)
                       let newList = listReplace sessions (_watchUserList st)
                       let newSt = st {_watchUserList=newList}
                       toScreen newSt WatchUserList
    EvKey KUp []    -> passToWidget st e watchMenu
    EvKey KDown []  -> passToWidget st e watchMenu
    EvKey KEsc []   -> toScreen st destination
    _ -> toScreen st destination
  where destination = if _authenticated st
                         then UserMenu
                         else MainMenu

handleWatchUserListEvent :: St -> Event -> EventM (Next St)
handleWatchUserListEvent st e =
  case e of
    EvKey KEnter [] -> suspendAndResume $ viewGame (listSelection st _watchUserList) st
    EvKey KUp []    -> passToWidget st e watchUserList
    EvKey KDown []  -> passToWidget st e watchUserList
    EvKey KEsc []   -> toScreen st WatchMenu
    _ -> toScreen st WatchMenu

handleNameEditEvent :: St -> Event -> EventM (Next St)
handleNameEditEvent st e =
  case e of
    EvKey KEnter [] -> case getEditInput _nameEditor st of
                         "" -> toScreen st MainMenu   -- if user doesn't supply username, go back to MainMenu
                         n  -> case st^.screen of
                                 NameEditorL -> toScreen newSt PassEditorL
                                 NameEditorR -> toScreen newSt PassEditorR
                                 where newSt = st { _userName = n }
    EvKey KEsc []   -> toScreen st MainMenu
    _ -> passToWidget st e nameEditor

handlePassEditEvent :: St -> Event -> EventM (Next St)
handlePassEditEvent st e =
  case e of
    EvKey KEnter [] -> case getEditInput _passEditor st of
                         "" -> toScreen st MainMenu
                         p  -> case st^.screen of
                                 PassEditorL -> logmein newSt
                                 PassEditorR -> toScreen newSt EmailEditor
                                 PassEditorC -> changepw newSt
                                 where newSt = st { _passWord = p }
    EvKey KEsc []   -> case st^.screen of
                         PassEditorC -> toScreen st UserMenu
                         _           -> toScreen st MainMenu
    _ -> passToWidget st e passEditor

handleEmailEditEvent :: St -> Event -> EventM (Next St)
handleEmailEditEvent st e =
  case e of
    EvKey KEnter [] -> toScreen newSt ConfDialog
                       where newSt = st { _emailAddr = getEditInput _emailEditor st}
    EvKey KEsc []   -> toScreen st MainMenu
    _ -> passToWidget st e emailEditor

handleConfDialogEvent st@(St{_userName = name, _passWord = pass, _emailAddr = mail}) e =
  case e of
    EvKey KEnter [] -> case dialogSelection (_confDialog st ) of
                         Just True -> do
                           result <- liftIO $ withConnection dbPath $ attemptRegister (pack name) (pack pass) (pack mail) -- TODO: extra function in Util.hs
                           if result >= 0 -- TODO: don't do this... pls
                              then continue $ st { _screen = UserMenu, _userID=result, _authenticated=True }
                              else toScreen st MainMenu
                         Just False -> toScreen st MainMenu
                         _ -> toScreen st MainMenu
    EvKey KEsc [] -> toScreen st MainMenu
    _ -> passToWidget st e confDialog

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
                                    newSt <- populateWatchMenu st
                                    toScreen newSt WatchMenu
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

startEvent :: St -> EventM St
startEvent = return

