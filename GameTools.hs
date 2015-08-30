{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module GameTools
       ( getGames,
         showGames,
         getGameNames,
         launchGame,
         getSessions,
         watchGame,
         Game(Game)
       ) where

import DBTools
import Data.Aeson
import Data.Text (Text, pack, unpack, replace)
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import System.Process
import System.Directory (removeFile, getDirectoryContents)

data Game = Game
      { shortName :: Text
      , gameName :: Text
      , rootPath :: Text
      , gamePath :: Text
      , userDir :: Text
      , sessionDir :: Text
      , templateCfg :: Text
      , cfgFile :: Text
      , gameArgs :: [Text]
      } deriving Show

instance FromJSON Game where
  parseJSON (Object v) = Game <$>
                         v .: "shortname" <*>
                         v .: "gamename" <*>
                         v .: "rootpath" <*>
                         v .: "gamepath" <*>
                         v .: "userdir" <*>
                         v .: "sessiondir" <*>
                         v .: "templatecfg" <*>
                         v .: "cfgfile" <*>
                         v .: "gameargs"
  parseJSON _           = mzero

getGames :: IO [Game]
getGames = do
  contents <- B.readFile "./config/games.json"
  return (checkGames (decode contents :: Maybe [Game]))
    where checkGames (Just games) = games
          checkGames Nothing = error "Could not read games.json!"

getGame :: String -> IO (Maybe Game)
getGame shortname = do
  games <- getGames
  return $ gameFromShort shortname games


gameFromShort :: String -> [Game] -> Maybe Game
gameFromShort shortname (game@(Game {shortName = gameName}):xs)
  | shortname == (unpack gameName) = Just game
  | otherwise = gameFromShort shortname xs
gameFromShort _ [] = Nothing

showGames :: [Game] -> [Text]
showGames ((Game {shortName = gameName}):xs) = gameName : showGames xs
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
launchGame :: String -> Text -> [Game] -> IO ()
launchGame s name (game@(Game shortname _ rootpath gamepath userdir sessiondir _ _ _ ):xs)
  | s == (unpack shortname) = do
      writeFile sessionFile ""
      callProcess (unpack  (replaceName (replaceRoot gamepath))) (makeArgs name game)
      removeFile sessionFile
  | otherwise = launchGame s name xs
        where makeArgs n (Game {gameArgs = args}) = replaceInArgs args rootpath userdir n
              replaceRoot = replace "%r" rootpath
              replaceName = replace "%u" name
              sessionFile = ((unpack (replaceRoot sessiondir))++(unpack name))
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
      where replaceRoot = replace "%r" (rootPath g)
            findSessions (Game {sessionDir=sd}) = (tail.tail) `fmap` getDirectoryContents (unpack (replaceRoot sd))
    Nothing -> return []

watchGame :: String -> IO ()
watchGame name = callProcess "tmux" args
  where args = ["a","-r", "-t", name]

{-
addPrivs :: UserField -> [String]
addPrivs user = addAdminArg user ++ addDebuggerArg user
  where addAdminArg (UserField _ username _ _ admin _) = if admin then ["--addadmin", unpack username] else []
        addDebuggerArg (UserField _ username _ _ _ debugger) = if debugger then ["--adddebugger", unpack username] else []
-}
