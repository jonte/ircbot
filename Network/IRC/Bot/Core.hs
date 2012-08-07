{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Network.IRC.Bot.Core
    ( simpleBot
    , simpleBot'
    , BotConf(..)
    , nullBotConf
    , User(..)
    , nullUser
    ) where

import Control.Concurrent       (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan  (Chan, dupChan, newChan, readChan, writeChan)
import Control.Concurrent.STM   (atomically)
import Control.Concurrent.STM.TMVar  (TMVar, swapTMVar, newTMVar, readTMVar)
import Control.Concurrent.QSem  (QSem, newQSem, signalQSem)
import Control.Exception        (IOException, catch)
import Control.Monad            (mplus, forever, when)
import Data.Set                 (Set, empty)
import Data.Time                (UTCTime, addUTCTime, getCurrentTime)
import Network                  (HostName, PortID(PortNumber), connectTo)
import Network.IRC              as I
import Network.IRC.Bot.Types    (User(..), nullUser)
import Network.IRC.Bot.Limiter  (Limiter(..), newLimiter, limit)
import Network.IRC.Bot.Log      (Logger, LogLevel(Normal, Debug), stdoutLogger)
import Network.IRC.Bot.BotMonad (BotPartT, BotEnv(..), runBotPartT)
import Prelude                  hiding (catch)
import System.IO                (BufferMode(LineBuffering), Handle, hClose, hGetLine, hPutStrLn, hSetBuffering)

-- |Bot configuration
data BotConf =
    BotConf
    { channelLogger :: Maybe (Chan Message -> IO ())  -- ^ optional channel logging function
    , logger        :: Logger           -- ^ app logging
    , host          :: HostName         -- ^ irc server to connect
    , port          :: PortID           -- ^ irc port to connect to (usually, 'PortNumber 6667')
    , nick          :: String           -- ^ irc nick
    , commandPrefix :: String           -- ^ command prefix
    , user          :: User             -- ^ irc user info
    , channels      :: Set String       -- ^ channel to join
    , limits        :: Maybe (Int, Int) -- ^ (burst length, delay in microseconds)
    }

nullBotConf :: BotConf
nullBotConf =
    BotConf { channelLogger  = Nothing
            , logger         = stdoutLogger Normal
            , host           = ""
            , port           = PortNumber 6667
            , nick           = ""
            , commandPrefix  = "#"
            , user           = nullUser
            , channels       = empty
            , limits         = Nothing
            }

-- | connect to irc server and send NICK and USER commands
ircConnect :: HostName -> PortID -> String -> User -> IO Handle
ircConnect host port _ _ =
    do h <- connectTo host port
       hSetBuffering h LineBuffering
       return h

partLoop :: Logger -> String -> String -> Chan Message -> Chan Message -> BotPartT IO () -> IO ()
partLoop logger botName pref incomingChan outgoingChan botPart =
  forever $ do msg <- readChan incomingChan
               runBotPartT botPart (BotEnv msg outgoingChan logger botName pref)

ircLoop :: Logger -> String -> String -> Chan Message -> Chan Message -> [BotPartT IO ()] -> IO [ThreadId]
ircLoop logger botName pref incomingChan outgoingChan =
    mapM forkPart
  where
    forkPart botPart =
      do inChan <- dupChan incomingChan
         forkIO $ partLoop logger botName pref inChan outgoingChan (botPart `mplus` return ())

-- reconnect loop is still a bit buggy
-- if you try to write multiple lines, and the all fail, reconnect will be called multiple times..
-- something should be done so that this does not happen
connectionLoop :: Logger -> Maybe (Int, Int) -> TMVar UTCTime -> HostName -> PortID -> String -> User -> Chan Message -> Chan Message -> Maybe (Chan Message) -> QSem -> IO (ThreadId, ThreadId, Maybe ThreadId, IO ())
connectionLoop logger mLimitConf tmv host port n u outgoingChan incomingChan logChan connQSem =
  do hTMVar <- atomically $ newTMVar (undefined :: Handle)
     (lim, limitTid) <-
         case mLimitConf of
           Nothing -> return (return (), Nothing)
           (Just (burst, delay)) ->
                    do limiter <- newLimiter burst delay
                       return (limit limiter, Just $ limitsThreadId limiter)
     doConnect logger host port n u hTMVar connQSem
     outgoingTid  <- forkIO $ forever $
                      do msg <- readChan outgoingChan
                         writeMaybeChan logChan msg
                         h <- atomically $ readTMVar hTMVar
                         when (msg_command msg `elem` ["PRIVMSG", "NOTICE"]) lim
                         hPutStrLn h (encode msg) `catch` reconnect logger host port n u hTMVar connQSem
                         now <- getCurrentTime
                         atomically $ swapTMVar tmv now
     incomingTid  <- forkIO $ forever $
                       do h <- atomically $ readTMVar hTMVar
                          msgStr <- hGetLine h `catch` (\e -> reconnect logger host port n u hTMVar connQSem e >> return "")
                          now <- getCurrentTime
                          atomically $ swapTMVar tmv now
                          case decode (msgStr ++ "\n") of
                            Nothing -> logger Normal ("decode failed: " ++ msgStr)
                            (Just msg) ->
                              do logger Debug (show msg)
                                 writeMaybeChan logChan msg
                                 writeChan incomingChan msg
     let forceReconnect =
             do putStrLn "forceReconnect 1"
                h <- atomically $ readTMVar hTMVar
                putStrLn "forceReconnect 2"
                writeChan outgoingChan (quit $ Just "restarting...")
                hClose h
                putStrLn "forceReconnect 3"
     return (outgoingTid, incomingTid, limitTid, forceReconnect)

ircConnectLoop :: (LogLevel -> String -> IO a)
     -> HostName -> PortID -> String -> User -> IO Handle
ircConnectLoop logger host port n u =
        ircConnect host port n u `catch`
        (\e ->
          do logger Normal $ "irc connect failed ... retry in 60 seconds: " ++ show (e :: IOException)
             threadDelay (60 * 10^(6::Integer))
             ircConnectLoop logger host port n u)

doConnect :: (LogLevel -> String -> IO a) -> String -> PortID -> String -> User -> TMVar Handle -> QSem -> IO ()
doConnect logger host port n u hTMVar connQSem =
    do logger Normal $ showString "Connecting to " . showString host . showString " as " $ n
       h <- ircConnectLoop logger host port n u
       atomically $ swapTMVar hTMVar h
       logger Normal "Connected."
       signalQSem connQSem
       return ()

reconnect :: Logger -> String -> PortID -> String -> User -> TMVar Handle -> QSem -> IOException -> IO ()
reconnect logger host port n u hTMVar connQSem e =
    do logger Normal $ "IRC Connection died: " ++ show e
       doConnect logger host port n u hTMVar connQSem

{- onConnectLoop :: Logger -> String -> String -> Chan Message -> QSem -> BotPartT IO () -> IO ThreadId
onConnectLoop logger botName pref outgoingChan connQSem action =
    forkIO $ forever $
      do waitQSem connQSem
         runBotPartT action (BotEnv undefined outgoingChan logger botName pref)
-}
-- |simpleBot connects to the server and handles messages using the supplied BotPartTs
--
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot :: BotConf          -- ^ Bot configuration
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', or equivalent)
          -> IO ([ThreadId], IO ())    -- ^ 'ThreadId' for all forked handler threads and a function that forces a reconnect
simpleBot BotConf{..} =
    simpleBot' channelLogger logger limits host port nick commandPrefix user 

-- |simpleBot' connects to the server and handles messages using the supplied BotPartTs
--
-- the 'Chan Message' for the optional logging function will include
-- all received and sent messages. This means that the bots output
-- will be included in the logs.
simpleBot' :: Maybe (Chan Message -> IO ()) -- ^ optional logging function
          -> Logger           -- ^ application logging
          -> Maybe (Int, Int) -- ^ rate limiter settings (burst length, delay in microseconds)
          -> HostName         -- ^ irc server to connect
          -> PortID           -- ^ irc port to connect to (usually, 'PortNumber 6667')
          -> String           -- ^ irc nick
          -> String           -- ^ command prefix
          -> User             -- ^ irc user info
          -> [BotPartT IO ()] -- ^ bot parts (must include 'pingPart', 'channelsPart', and 'nickUserPart)'
          -> IO ([ThreadId], IO ())    -- ^ 'ThreadId' for all forked handler threads and an IO action that forces a reconnect
simpleBot' mChanLogger logger limitConf host port n pref u parts =
  do (mLogTid, mLogChan) <-
         case mChanLogger of
           Nothing  -> return (Nothing, Nothing)
           (Just chanLogger) ->
               do logChan <- newChan :: IO (Chan Message)
                  logTid  <- forkIO $ chanLogger logChan
                  return (Just logTid, Just logChan)
     -- message channels
     outgoingChan <- newChan :: IO (Chan Message)
     incomingChan <- newChan :: IO (Chan Message)
     now <- getCurrentTime
     tmv <- atomically $ newTMVar now
     connQSem <- newQSem 0
     (outgoingTid, incomingTid, mLimitTid, forceReconnect) <- connectionLoop logger limitConf tmv host port n u outgoingChan incomingChan mLogChan connQSem
     watchDogTid <- forkIO $ forever $
                    do let timeout = 5*60
                       now'         <- getCurrentTime
                       lastActivity <- atomically $ readTMVar tmv
                       when (now' > addUTCTime (fromIntegral timeout) lastActivity) forceReconnect
                       threadDelay (30*10^(6::Integer)) -- check every 30 seconds
     ircTids     <- ircLoop logger n pref incomingChan outgoingChan parts
     -- onConnectId <- onConnectLoop logger n pref outgoingChan connQSem onConnect
     return (maybe id (:) mLimitTid $ maybe id (:) mLogTid (incomingTid : outgoingTid : watchDogTid : ircTids), forceReconnect)
    {-where
      onConnect :: BotPartT IO ()
      onConnect =
          changeNickUser n (Just u)-}

-- | call 'writeChan' if 'Just'. Do nothing for Nothing.
writeMaybeChan :: Maybe (Chan a) -> a -> IO ()
writeMaybeChan Nothing     _ = return ()
writeMaybeChan (Just chan) a = writeChan chan a
