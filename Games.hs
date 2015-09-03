{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Games (myGames) where

import Types

myGames :: [Game]
myGames = theGames

theGames :: [Game]
theGames = [ Game { shortname   = "CDDA"
                  , gamename    = "Cataclysm: Dark Days Ahead"
                  , rootpath    = "/home/codehero/hgametest/"
                  , gamepath    = "%r/cdda/cataclysm"
                  , userdir     = "%ruserdata/cdda/%n/"
                  , sessiondir  = "%rsessions/cdda/"
                  , templatecfg = "%rcdda/template.cfg"
                  , cfgfile     = "%uconfig/options.txt"
                  , gameargs    = [ "--username", "%n"
                                  , "--basepath", "%r"
                                  , "--userdir", "%u"
                                  , "--datadir", "%rshare/cataclysm-dda/"]}
           , Game { shortname   = "CDDA SHARED"
                  , gamename    = "Cataclysm: Dark Days Ahead (Shared)"
                  , rootpath    = "!rootpath"
                  , gamepath    = "%r/cdda/cataclysm"
                  , userdir     = "%ruserdata/cdda-shared/%n/"
                  , sessiondir  = "%rsessions/cdda-shared/"
                  , templatecfg = "%rcdda/template.cfg"
                  , cfgfile     = "%uconfig/options.txt"
                  , gameargs    = [ "--username", "%n"
                                  , "--basepath", "%r"
                                  , "--userdir", "%u"
                                  , "--savedir", "%r/share/save/"
                                  , "--memorialdir", "%r/share/memorial/"
                                  ,  "--datadir", "%rshare/cataclysm-dda/"
		                  , "--shared" ]}]
