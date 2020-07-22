module Interview.Class.WebSocket where

import           Network.WebSockets (WebSocketsData)
import           Network.WebSockets.Connection (Connection)
import qualified Network.WebSockets.Connection as WS


newtype PingInterval = PingInterval Int
    deriving (Show, Eq, Ord, Num)

class (Monad m) => WebSocket m where
    forkPingThread
        :: Connection
        -> PingInterval
        -> m ()


    sendTextDatas
        :: (WebSocketsData a)
        => Connection
        -> [a]
        -> m ()

instance WebSocket IO where
    forkPingThread
        :: Connection
        -> PingInterval
        -> IO ()
    forkPingThread conn (PingInterval interval) =
        WS.forkPingThread conn interval


    sendTextDatas
        :: (WebSocketsData a)
        => Connection
        -> [a]
        -> IO ()
    sendTextDatas conn msgs =
        WS.sendTextDatas conn msgs
