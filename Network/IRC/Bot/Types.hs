{-# LANGUAGE DeriveDataTypeable #-}
module Network.IRC.Bot.Types
    ( User(..)
    , nullUser
    ) where

import Data.Data                (Data, Typeable)
import Network                  (HostName)
import Prelude                  hiding (catch)


data User = User
    { username   :: String    -- ^ username on client system
    , hostname   :: HostName  -- ^ hostname of client system
    , servername :: HostName  -- ^ irc server client is connected to
    , realname   :: String    -- ^ client's real name
    }
    deriving (Data, Typeable, Eq, Ord, Read, Show)

nullUser :: User
nullUser = User { username   = ""
                , hostname   = "."
                , servername = "."
                , realname   = ""
                }
