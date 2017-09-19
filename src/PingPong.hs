{-# LANGUAGE DeriveGeneric #-}
module PingPong where

import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics

data PMessage = Ping ProcessId
    | Pong ProcessId
    deriving (Typeable, Generic)

instance Binary PMessage

pingServer :: Process ()
pingServer = do
    Ping from <- expect
    -- say $ printf "ping received from %s" (show from)
    mypid <- getSelfPid
    send from (Pong mypid)

