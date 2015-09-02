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
    WatchGames  -> handleGameMenuEvent st e
    WatchUsers -> handleWatchUsersEvent st e
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
                                     newSt <- populateGameMenu st
                                     toScreen newSt WatchGames
                         "logout" ->  halt st
                         _ -> halt st
    EvKey KUp []   -> passToWidget st e mainMenu
    EvKey KDown [] -> passToWidget st e mainMenu
    EvKey KEsc []  -> halt st
    _ -> continue st
   -- implement error logging

handleWatchUsersEvent :: St -> Event -> EventM (Next St)
handleWatchUsersEvent st e =
  case e of
    EvKey KEnter [] -> suspendAndResume $ viewGame (listSelection st _watchUsers) st
    EvKey KUp []    -> passToWidget st e watchUsers
    EvKey KDown []  -> passToWidget st e watchUsers
    EvKey KEsc []   -> toScreen st WatchGames
    _ -> toScreen st WatchUsers

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
                           registerme st
                         Just False -> toScreen st MainMenu
                         _ -> toScreen st MainMenu
    EvKey KEsc [] -> toScreen st MainMenu
    _ -> passToWidget st e confDialog

handleUserMenuEvent :: St -> Event -> EventM (Next St)
handleUserMenuEvent st e =
  case e of
    EvKey KEnter [] -> case listSelection st _userMenu of
                         "play game" -> do
                           newSt <- populateGameMenu st
                           toScreen newSt GameMenu
                         "watch" -> do
                           newSt <- populateGameMenu st
                           toScreen newSt WatchGames
                         "change pw" -> toScreen st PassEditorC
                         "logout" -> halt st
                         _ -> halt st
    EvKey KUp []   -> passToWidget st e userMenu
    EvKey KDown [] -> passToWidget st e userMenu
    _ -> continue st

handleGameMenuEvent :: St -> Event -> EventM (Next St)
handleGameMenuEvent st e =
  case e of
    EvKey KEnter [] -> case st^.screen of
                         GameMenu -> suspendAndResume $ startGame (listSelection st _gameMenu) st
                         WatchGames -> do
                          sessions <- liftIO $ getSessions (listSelection st _gameMenu)
                          _ <- liftIO $ writeFile "./leSession.txt" (show sessions)
                          let newSt = st {_watchUsers = listReplace sessions (_watchUsers st)}
                          toScreen newSt WatchUsers
    EvKey KUp []  -> passToWidget st e gameMenu
    EvKey KDown [] -> passToWidget st e gameMenu
    EvKey KEsc [] -> toScreen st destination
    _ -> continue st
    where destination = if _authenticated st
                           then UserMenu
                           else MainMenu
--handlePassEditEvent :: St -> Event -> EventM (Next St)
-- those are going to be very similar. combine them into one function handleEditEvent

startEvent :: St -> EventM St
startEvent = return
