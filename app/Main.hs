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

import Options.Applicative

data AppSettings = AppSettings { server :: String
                 , port :: Int
                 , chan :: String
                 , nick :: String }

defaultSettings = AppSettings { server = "irc.freenode.org"
                              , port   = 6667 }
-- , chan   = "##itb"
-- , nick   = "clonebot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle
               , botSettings :: AppSettings }
 
-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = execParser opts >>= \settings -> bracket (connect settings) disconnect (loop settings)
  where
    disconnect = hClose . socket
    loop settings st    = runReaderT (run settings) st
    opts = info parser idm
    parser = AppSettings <$> argument str (metavar "SERVER")
                         <*> argument auto (metavar "PORT")
                         <*> argument str (metavar "CHAN")
                         <*> argument str (metavar "NICK")
 
-- Connect to the server and return the initial bot state
connect :: AppSettings -> IO Bot
connect settings = notify $ do
    h <- connectTo (server settings) (PortNumber (fromIntegral (port settings)))
    hSetBuffering h NoBuffering
    return (Bot h settings)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " (server settings) >> hFlush stdout)
        (putStrLn "done.")
        a
 
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: AppSettings -> Net ()
run settings = do
    write "NICK" (nick settings)
    write "USER" ((nick settings) ++" 0 * :tutorial bot")
    write "JOIN" (chan settings)
    asks socket >>= listen
 
-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s >> hFlush stdout)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)


data Command = Command { commandName :: String
                       , commandArg :: String -> Net ()
                       }

commands :: [Command]
commands = [ Command "!help"                (\_ -> privmsg "Usage: !please-help")
           , Command "!please-help"         (\_ -> privmsg pleaseHelp)
           , Command "!lunchy-munchy"       (\_ -> messageProcess "./LunchParse")
           , Command "!is-it-safe-outside?" (\a -> messageProcessA "./WeatherParse" (words a))
           , Command "!what-the-load"       (\_ -> messageProcess "./MonitParse")
           , Command "!what-the-swap"       (\_ -> messageProcess "./MemParse")
           , Command "!what-the-dickens"    (\_ -> messageProcess "./BookPrint")
           , Command "!what-the-math"       (messageLambda)
           ]

pleaseHelp = "Available commands: " ++ (intercalate " " (fmap commandName commands)) ++ "\nSee http://blog.itb.pri for more info."

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval msg                       = maybe (return ()) (applyCommand msg) (find (\c -> (commandName c) `isPrefixOf` msg) commands)
  where
    applyCommand :: String -> Command -> Net ()
    applyCommand msg cmd = (commandArg cmd) (drop ((length $ commandName cmd) + 1) msg)

messageLambda :: String -> Net ()
messageLambda math = do
    (_, Just hout, Just herr, jHandle) <- io $ createProcess (proc "/usr/bin/cabal" ["exec", "lambdabot", "--", "-e", "> " ++ math])
                                              { cwd = Just "/home/schuppner/Projects/lambdabot/lambdabot"
                                              , std_out = CreatePipe
                                              , std_err = CreatePipe }
    (io $ hGetContents hout) >>= privmsg

    exitCode <- io $ waitForProcess jHandle
    io $ putStrLn $ "Exit code: " ++ show exitCode
    io $ hFlush stdout

messageProcess :: String -> Net ()
messageProcess cmd = messageProcessA cmd []

messageProcessA :: String -> [String] -> Net ()
messageProcessA cmd args = do
    (_, Just hout, Just herr, jHandle) <- io $ createProcess (proc cmd args)
                                              { cwd = Just "."
                                              , std_out = CreatePipe
                                              , std_err = CreatePipe }
    (io $ hGetContents hout) >>= privmsg

    exitCode <- io $ waitForProcess jHandle
    io $ putStrLn $ "Exit code: " ++ show exitCode
    io $ hFlush stdout
 
-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = do
    settings <- asks botSettings
    let ch = chan settings
    mapM_ (\l -> write "PRIVMSG" (ch ++ " :" ++ l)) $ lines s
 
-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
    io $ threadDelay $ round $ secondsDelay * 1000000
  where
    secondsDelay = 1
 
-- Convenience.
io :: IO a -> Net a
io = liftIO
