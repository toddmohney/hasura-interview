module RouteHandlers.Health where

import           Control.Monad (forever)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import           Network.WebSockets.Connection (Connection)

import           Interview.Class.Concurrency (Concurrency(..))
import           Interview.Class.Instrumentation (Instrumentation(..))
import           Interview.Class.WebSocket (WebSocket(..))
import           Interview.ServerStats (fromValue)


serverHealth
    :: ( Concurrency m
       , Instrumentation m
       , WebSocket m
       )
    => Connection
    -> m ()
serverHealth conn = do
    -- We ping the client every 10 seconds to prevent unintended connection
    -- hangup by, say, a reverse proxy.
    forkPingThread conn 10

    forever $ do
        serverStats <- sampleAll
        sendTextDatas conn [jsonEncodedStats serverStats]
        sleep 2
  where
    jsonEncodedStats =
        TE.decodeUtf8
            . BSL.toStrict
            . AE.encode
            . fromValue
