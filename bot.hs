{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Text.Printf
import System.Process
import System.Environment ( getArgs )

import Control.Applicative

mkConnection c n = (mkDefaultConfig "irc.freenode.org" n)
  { cChannels = [c]
  , cEvents = events
  }

-- Set up actions to run on start and end, and run the main loop
main = do
    [c, n] <- getArgs
    let connection = mkConnection c n
    connect connection False True

-- Dispatch a command
onMessage :: EventFunc
onMessage s m
  | msg == "!quit"             = putStrLn ":Exiting" >> (exitWith ExitSuccess)
  | msg == "!help"             = sendMsg s chan "help yourself"
  | msg == "!lunchy-munchy"    = do
        menu <- getMessageProcess "./LunchParse"
        sendMsg s chan (B.pack menu)
  | "!id " `B.isPrefixOf` msg = sendMsg s chan (B.drop 4 msg)
  | otherwise                   = return () -- ignore everything else
  where chan = fromJust $ mChan m
        msg = mMsg m

events = [(Privmsg onMessage)]


getMessageProcess :: String -> IO String
getMessageProcess cmd = do
    (_, Just hout, Just herr, jHandle) <- createProcess (proc cmd [])
                                              { cwd = Just "."
                                              , std_out = CreatePipe
                                              , std_err = CreatePipe }
    exitCode <- waitForProcess jHandle
    putStrLn $ "Exit code: " ++ show exitCode

    hGetContents hout


