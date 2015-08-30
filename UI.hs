{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module UI
  ( draw
  ) where

import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.Widgets.Dialog

import Data.Text

import Types
{-
draw :: St -> [Widget]
--draw st@(St MainList _ _ _ _)  = inBox st _mainList
--                                   "Welcome to Hgamelaunch."
--                                   "Enter to choose. Esc to cancel."
draw st@(St{_screen = MainList}) = box st drawList _mainList
                                   "Welcome, you."
                                   "Enter to choose. Esc to cancel."
draw st@(St{_screen = WatchList}) = box st drawList _watchList "watch" "watch"
draw st@(St{_screen = UserEditor}) = box st drawEdit _userEditor "blubb" "blubb"
draw st@(St{_screen = PassEditor}) = box st drawEdit _passEditor "ping" "pong"
draw st@(St{_screen = UserMenu}) = box st drawList _userMenu "User menu" ""
-}
draw :: St -> [Widget]
draw st@(St{_screen = s}) = 
  case s of
    MainList   -> boxList _mainList "Welcome, you" "Enter to choose. Esc to cancel"
    WatchList  -> boxList _watchList "Which game would you like to watch?" ""
    WatchUserList -> boxList _watchUserList "Select a user." "Press \"Ctrl-b d\" to exit view mode."
    NameEditorL -> boxEdit _nameEditorL "Enter your username." ""
    PassEditorL -> boxEdit _passEditorL "Enter your password." ""
    NameEditorR -> boxEdit _nameEditorR "Enter a username." ""
    PassEditorR -> boxEdit _passEditorR "Enter a password." ""
    EmailEditor -> boxEdit _emailEditor "Enter your email." "If you want, that is."
    ConfDialogR -> [drawDialog st _confDialogR ("username: " ++ _userName st
                                               ++ "password: " ++ _passWord st)]
    PassEditorC -> boxEdit _passEditorC "Enter your new password." ""
    UserMenu   -> boxList _userMenu (str ("hello "++ _userName st)) ""
    GameMenu   -> boxList _gameMenu "What do you want to play?" ""
    _ -> []
    where boxList = box st drawList
          boxEdit = box st drawEdit

-- TODO: either change function name or box in where clause
box :: t -> (t -> a -> Widget) -> a -> Widget -> Widget -> [Widget]
box st f a header footer = [ui]
  where box' = borderWithLabel header $
               hLimit 40 $
               vLimit 15 $
               f st a
        ui = vCenter $ vBox [ hCenter box'
                            , hCenter footer]

{-
inBox :: t -> (t -> List e) -> Widget -> Widget -> [Widget]
inBox st l header footer = [ui]
  where box = borderWithLabel header $
              hLimit 40 $
              vLimit 15 $
              drawList st l
        ui = vCenter $ vBox [ hCenter box
                            , hCenter footer]

inBoxEditor st e header footer = [ui]
  where box = borderWithLabel header $
              hLimit 40 $
              vLimit 15 $
              drawEditor st e
        ui = vCenter $ vBox [ hCenter box
                            , hCenter footer]
-}

drawList :: t -> (t -> List e) -> Widget
drawList st l = renderList $ l st

drawEdit :: t -> (t -> Editor) -> Widget
drawEdit st e = renderEditor $ e st

drawDialog :: t -> (t -> Dialog e) -> String -> Widget
drawDialog st d s = renderDialog  (d st) $ hCenter $ padAll 1 $ str s
