{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GameTools
       ( getGames,
         showGames,
         getGameNames,
         launchGame,
         getSessions,
         watchGame,
         myGames,
         writeGames,
         viewGame,
         startGame,
         Game(Game)
       ) where

import DBTools
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Text (Text, pack, unpack, replace)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import System.Process
import System.Directory (removeFile, getDirectoryContents, doesFileExist)

import Types
import Games


writeGames = B.writeFile "games.txt" (encodePretty myGames)

getGames :: IO [Game]
getGames = do
  contents <- B.readFile "./config/games.json" -- TODO: don't hardcode
  return (checkGames (decode contents :: Maybe [Game]))
    where checkGames (Just games) = games
          checkGames Nothing = []

getGame :: String -> IO (Maybe Game)
getGame shortname = do
  games <- getGames
  return $ gameFromShort shortname games


gameFromShort :: String -> [Game] -> Maybe Game
gameFromShort shortn (game@(Game {shortname = gameName}):xs)
  | shortn == (unpack gameName) = Just game
  | otherwise = gameFromShort shortn xs
gameFromShort _ [] = Nothing

showGames :: [Game] -> [Text]
showGames ((Game {shortname = gameName}):xs) = gameName : showGames xs
showGames [] = []
  
getGameNames :: IO [String]
getGameNames = do
  games <- getGames
  let gameNames = map unpack (showGames games)
  return gameNames

{-
launchGame :: Int -> Text -> [Game] -> IO ()
launchGame n name (game@(Game choice _ rootpath gamepath userdir _ _ _ _ _):xs)
  | (cons c "") == choice = do
      callProcess (unpack (replace "%r" rootpath gamepath)) (makeArgs name game)
  | otherwise = launchGame c name xs
        where makeArgs n (Game {gameArgs = args}) = replaceInArgs args rootpath userdir n
--                                                       ++ addPrivs u
launchGame _ _ _ = return ()
-}
launchGame :: String -> Text -> [Game] -> IO () -- TODO: clean up. need better variable replacement
launchGame s name (game@(Game shortname _ rootpath gamepath userdir sessiondir _ _ _ ):xs)
  | s == (unpack shortname) = do
      exists <- doesFileExist sessionFile
      if not exists
         then newGame
         else attachGame
      removeFile sessionFile
  | otherwise = launchGame s name xs
    where newGame = do
                  writeFile sessionFile ""
                  callProcess "tmux" $ ["new", "-s", (unpack name)] ++ [(unpack . replaceName $ replaceRoot gamepath)++ " "++(unwords $ makeArgs name game)]
          attachGame = do
                  removeFile sessionFile
                  callProcess "tmux" ["a", "-t", (unpack name)]
                  writeFile sessionFile ""
          makeArgs n (Game {gameargs = args}) = replaceInArgs args rootpath userdir n
          replaceRoot = replace "%r" rootpath
          replaceName = replace "%u" name
          sessionFile = (unpack $ replaceRoot sessiondir)++(unpack name)
launchGame _ _ _ = return ()

replaceInArgs :: [Text] -> Text -> Text -> Text -> [String]
replaceInArgs (x:xs) rootpath userdir username =
  unpack (replaceVars  x) : replaceInArgs xs rootpath userdir username
  where replaceVars = replace "%n" username . replace "%r" rootpath . replace "%u" userdir
replaceInArgs [] _ _ _ = []

getSessions :: String -> IO [String]
getSessions shortname = do
  games <- getGames
  let game = gameFromShort shortname games
  case game of
    Just g -> findSessions g
      where replaceRoot = replace "%r" (rootpath g)
            findSessions (Game {sessiondir=sd}) = (tail.tail) `fmap` getDirectoryContents (unpack (replaceRoot sd))
    Nothing -> return []

watchGame :: String -> IO ()
watchGame name = callProcess "tmux" args
  where args = ["a","-r", "-t", name]

startGame :: String -> St -> IO St
startGame sel st = do
   games <- getGames
   case games of
     [] -> return st {_screen = UserMenu}
     _  -> do 
           launchGame sel (pack(_userName st)) games
           return st

viewGame :: String -> St -> IO St
viewGame sel st = do
   watchGame sel
   return st

{-
addPrivs :: UserField -> [String]
addPrivs user = addAdminArg user ++ addDebuggerArg user
  where addAdminArg (UserField _ username _ _ admin _) = if admin then ["--addadmin", unpack username] else []
        addDebuggerArg (UserField _ username _ _ _ debugger) = if debugger then ["--adddebugger", unpack username] else []
-}
