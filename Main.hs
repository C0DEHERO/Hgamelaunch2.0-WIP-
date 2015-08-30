{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Monoid
import Control.Monad (void)

import Brick.Main
import Brick.Util
import Brick.AttrMap
import Brick.Widgets.List
import Brick.Widgets.Dialog

import Graphics.Vty

import System.Directory

import Database.SQLite.Simple (open, close)

import Types
import UI
import Events
import DBTools (makeDb, dbPath)

import GameTools


{-
data Event =
    VtyEvent Event
    | ClearStatus String
    
    
drawMainList :: St -> Widget
drawMainList (St {_mainList = l}) = renderList l

drawWatchList :: St -> Widget
drawWatchList (St {_watchList = l}) = renderList l
-}
app :: App St Event
app =
    App { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = startEvent
        , appAttrMap = const attributeMap
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
     exists <- doesFileExist dbPath
     conn <- open dbPath
     makeDb conn exists
     close conn
     void $ defaultMain app initialState


attributeMap :: AttrMap
attributeMap = attrMap defAttr
    [ (listAttr,         white `on` blue)
    , (listSelectedAttr, blue `on` white)
    , (dialogAttr,       white `on` blue)
    , (buttonAttr,       white `on` blue)
    , (buttonSelectedAttr,       blue `on` white)
    , (customAttr,        fg cyan)]
    where customAttr = listSelectedAttr <> "custom"

