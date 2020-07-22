{-# OPTIONS_GHC -fno-warn-orphans #-}

module Interview.Orphans
    ( Stats(..)
    , WebSocket
    ) where

import           Data.Aeson.TH
import           Servant.API.WebSocket (WebSocket)
import           Servant.Ekg (HasEndpoint(..))
import           System.Metrics.Distribution (Stats(..))

$(deriveJSON defaultOptions ''Stats)


{-
 - This instance attempts to convince our EKG stats collector to ignore
 - stats on the websocket endpoint(s).
 - https://hackage.haskell.org/package/servant-ekg-0.3.1/docs/Servant-Ekg.html
 -}
instance HasEndpoint WebSocket where
    getEndpoint _proxy _request =
        Nothing

    enumerateEndpoints _proxy =
        []
