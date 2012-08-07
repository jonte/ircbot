{-# LANGUAGE DeriveDataTypeable #-}
module Network.IRC.Bot.Commands where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.List (isPrefixOf)
import Network (HostName)
import Network.IRC
import Network.IRC.Bot.BotMonad

-- * Commands

cmd :: (Functor m, MonadPlus m, BotMonad m) => Command -> m ()
cmd cmdName =
  do c <- msg_command <$> askMessage
     unless (cmdName == c) mzero

data Ping
  = Ping HostName
  deriving (Eq, Ord, Read, Show, Data, Typeable)

ping :: (Functor m, MonadPlus m, BotMonad m) => m Ping
ping =
  do cmd "PING"
     params <- msg_params  <$> askMessage
     case params of
       (hostName:_) -> return $ Ping hostName
       _ -> mzero


data PrivMsg
  = PrivMsg { prefix     :: Maybe Prefix
            , receivers  :: [String]
            , msg        :: String
            }
      deriving (Eq, Read, Show)

privMsg :: (Functor m, MonadPlus m, BotMonad m) => m PrivMsg
privMsg =
  do m <- askMessage
     maybe mzero return (toPrivMsg m)

toPrivMsg :: Message -> Maybe PrivMsg
toPrivMsg m =
  let c      = msg_command m
      params = msg_params  m
      pref   = msg_prefix  m
  in case c of
      "PRIVMSG" -> Just $ PrivMsg pref   (init params) (last params)
      _         -> Nothing

class ToMessage a where
  toMessage :: a -> Message

sendCommand :: (ToMessage c, BotMonad m, Functor m) => c -> m ()
sendCommand c = sendMessage (toMessage c)

data Pong
  = Pong HostName
      deriving (Eq, Ord, Read, Show, Data, Typeable)

instance ToMessage Pong where
    toMessage (Pong hostName) = Message Nothing "PONG" [hostName]

instance ToMessage PrivMsg where
    toMessage (PrivMsg pref recvs m) = Message pref "PRIVMSG" (recvs ++ [m])


-- | get the nickname of the user who sent the message
askSenderNickName :: (BotMonad m) => m (Maybe String)
askSenderNickName =
    do m <- askMessage
       case msg_prefix m of
         (Just (NickName n _ _)) -> return (Just n)
         _ -> return Nothing

-- | figure out who to reply to for a given `Message`
--
-- If message was sent to a #channel reply to the channel. Otherwise reply to the sender.
replyTo :: (BotMonad m) => m (Maybe String)
replyTo =
    do priv <- privMsg
       let receiver = head (receivers priv)
       if "#" `isPrefixOf` receiver
          then return (Just receiver)
          else askSenderNickName

-- | returns the receiver of a message
--
-- if multiple receivers, it returns only the first
askReceiver :: (Alternative m, BotMonad m) => m (Maybe String)
askReceiver =
    do priv <- privMsg
       return (Just (head $ receivers priv))
    <|> return Nothing

