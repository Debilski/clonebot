import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.List
import Network
import Options.Applicative
import System.Exit
import System.IO
import System.Process
import System.Systemd.Daemon (notifyWatchdog)
import Text.Printf

data AppSettings = AppSettings { server :: String
                 , port :: Int
                 , chan :: String
                 , nick :: String }

defaultSettings = AppSettings { server = "irc.freenode.org"
                              , port   = 6667 }
-- , chan   = "##itb"
-- , nick   = "clonebot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = StateT Bot IO
data Bot = Bot { socket :: Handle
               , botSettings :: AppSettings }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = execParser opts >>= \settings -> bracket (connect settings) disconnect (loop settings)
  where
    disconnect = hClose . socket
    loop settings st    = evalStateT (run settings) st
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
    write "USER" ((nick settings) ++" 0 * :generic itb bot")
    write "JOIN" (chan settings)
    gets socket >>= listen


reconnect :: Net ()
reconnect = do
    io $ putStrLn ("Reconnecting in " ++ (show delay) ++ " seconds")
    io $ threadDelay (delay * 1000000)
    h <- gets socket
    s <- gets botSettings
    io $ hClose h
    b <- io $ connect s
    put b
  where
    delay = 3


readInputLine :: Handle -> IO (Maybe String)
readInputLine h = do
    res <- try $ init <$> hGetLine h
    case res of
      Right l                -> return (Just l)
      Left (SomeException _) -> return Nothing


timeoutAndPing :: Handle -> Int -> Int -> String -> IO (Maybe String)
timeoutAndPing h lag time msg = do
    threadDelay (lag * 1000000)
    ping h msg
    threadDelay (time * 1000000)
    putStrLn "Timed out..."
    hFlush stdout
    return Nothing

ping :: Handle -> String -> IO ()
ping h = writeIO h "PING"

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    a <- gets botSettings
    msg <- io $ async $ readInputLine h
    timeout <- io $ async $ timeoutAndPing h 60 10 (nick a)
    r <- io $ waitAnyCancel [msg, timeout]
    case r of
      (_, Just s) -> do

          io (putStrLn s >> hFlush stdout)
          io notifyWatchdog
          if ping s then pong s else eval (clean s)
      (_, Nothing) -> reconnect
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
           , Command "!about"               (\_ -> privmsg about)
           , Command "!please-help"         (\_ -> privmsg pleaseHelp)
           , Command "!lunchy-munchy"       (\_ -> messageProcess "./LunchParse")
           , Command "!is-it-safe-outside?" (\a -> messageProcessA "./WeatherParse" (words a))
           , Command "!is-it-save-outside?" (\_ -> privmsg "Did you mean: !is-it-safe-outside?")
           , Command "!tip-of-the-day"      (\_ -> messageProcess "./TipOfTheDay")
           , Command "!what-the-load"       (\_ -> messageProcess "./MonitParse")
           , Command "!what-the-swap"       (\_ -> messageProcess "./MemParse")
           , Command "!what-the-dickens"    (\_ -> messageProcess "./BookPrint")
           , Command "!mitesser"            (\_ -> messageProcessA "/bin/sh" ["-c", "curl -s  https://www.cms.hu-berlin.de/de/dl/netze/wlan/stats/details/Berlin-MitteCampusNordHannoverscheStr7MensaNord.html | grep Aktive |sed -r \"s/[^0-9]//g\""])
           , Command "!euro2016-ranks"      (\_ -> messageProcessA "/bin/bash" ["-c", "xml_grep --html --text_only '*/td[@class=\"mg_class\"]' <(/usr/bin/curl https://www.kicktipp.de/itb-supertippers/gesamtuebersicht) | head -5 | nl"])
           , Command "!what-the-math"       (messageLambda)
           ]

pleaseHelp = "Available commands: " ++ (intercalate " " (fmap commandName commands)) ++ "\nSee http://blog.itb.pri for more info."

about = "ITB clonebot. Source code and issue tracker @ https://github.com/Debilski/clonebot/"

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
    (_, Just hout, Just herr, jHandle) <- io $ createProcess (proc "/home/schuppner/.local/bin/stack" ["exec", "lambdabot", "--", "-e", "> " ++ math])
                                              { cwd = Just "/home/schuppner/Projects/lambdabot/stack/simple"
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
    settings <- gets botSettings
    let ch = chan settings
    mapM_ (\l -> write "PRIVMSG" (ch ++ " :" ++ l)) $ lines s

writeIO :: Handle -> String -> String -> IO ()
writeIO h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- gets socket
    io $ writeIO h s t
    io $ notifyWatchdog
    io $ threadDelay $ round $ secondsDelay * 1000000
  where
    secondsDelay = 1

-- Convenience.
io :: IO a -> Net a
io = liftIO
