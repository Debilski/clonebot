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

server = "irc.freenode.org"
port   = 6667
chan   = "##itb"
-- chan   = "##itbtestmybot"
nick   = "clonebot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }
 
-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st
 
-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
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
                       , commandArg :: Net ()
                       }

commands :: [Command]
commands = [ Command "!help"                (privmsg "help yourself")
           , Command "!please-help"         (privmsg $ intercalate " " (fmap commandName commands))
           , Command "!lunchy-munchy"       (messageProcess "./LunchParse")
           , Command "!is-it-safe-outside?" (messageProcess "./WeatherParse")
           , Command "!what-the-load"       (messageProcess "./MonitParse")
           , Command "!what-the-swap"       (messageProcess "./MemParse")
           , Command "!what-the-dickens"    (messageProcess "./BookPrint")
           ]

-- Dispatch a command
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval command                   = case find (\c -> commandName c == command) commands of
                                   Just command -> commandArg command
                                   Nothing -> return () -- ignore everything else

messageProcess :: String -> Net ()
messageProcess cmd = do
    (_, Just hout, Just herr, jHandle) <- io $ createProcess (proc cmd [])
                                              { cwd = Just "."
                                              , std_out = CreatePipe
                                              , std_err = CreatePipe }
    (io $ hGetContents hout) >>= privmsg

    exitCode <- io $ waitForProcess jHandle
    io $ putStrLn $ "Exit code: " ++ show exitCode
    io $ hFlush stdout
 
-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = mapM_ (\l -> write "PRIVMSG" (chan ++ " :" ++ l)) $ lines s
 
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

