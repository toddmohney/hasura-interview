module ServerStats where

import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import           GHC.Int (Int64)
import           Orphans (Stats(..))
import           System.Metrics (Sample, Value(..))


data ResponseStatusCount = ResponseStatusCount
    { fiveHundreds :: Int64
    , fourHundreds :: Int64
    , twoHundreds :: Int64
    }

$(deriveJSON defaultOptions ''ResponseStatusCount)


data ResponseCount = ResponseCount
    { totalResponseCount :: Int64
    , responseStatusCounts :: ResponseStatusCount
    , responseTimeDistribution :: Maybe Stats
    }

$(deriveJSON defaultOptions ''ResponseCount)


data ServerStats = ServerStats
    { inFlightRawRequests :: Int64
    , rawResponsesStats :: ResponseCount
    , inFlightGETRequests :: Int64
    , getResponsesStats :: ResponseCount
    }

$(deriveJSON defaultOptions ''ServerStats)


fromValue
    :: Sample
    -> ServerStats
fromValue sample = ServerStats
    { inFlightRawRequests = getGaugeValue "servant.path.public.RAW.in_flight"
    , rawResponsesStats = ResponseCount
        { totalResponseCount = getCounterValue "servant.path.public.RAW.responses.XXX"
        , responseStatusCounts = ResponseStatusCount
            { fiveHundreds = getCounterValue "servant.path.public.RAW.responses.5XX"
            , fourHundreds = getCounterValue "servant.path.public.RAW.responses.4XX"
            , twoHundreds = getCounterValue "servant.path.public.RAW.responses.2XX"
            }
        , responseTimeDistribution = getDistributionStats "servant.path.public.RAW.time_ms"
        }
    , inFlightGETRequests = getGaugeValue "servant.path.GET.in_flight"
    , getResponsesStats = ResponseCount
        { totalResponseCount = getCounterValue "servant.path.GET.responses.XXX"
        , responseStatusCounts = ResponseStatusCount
            { fiveHundreds = getCounterValue "servant.path.GET.responses.5XX"
            , fourHundreds = getCounterValue "servant.path.GET.responses.4XX"
            , twoHundreds = getCounterValue "servant.path.GET.responses.2XX"
            }
        , responseTimeDistribution = getDistributionStats "servant.path.GET.time_ms"
        }
    }
  where
    getGaugeValue key =
        case HM.lookup key sample of
            Nothing ->
                errVal

            (Just (Gauge val)) ->
                val

            (Just _) ->
                errVal

    getCounterValue key =
        case HM.lookup key sample of
            Nothing ->
                errVal

            (Just (Counter val)) ->
                val

            (Just _) ->
                errVal

    getDistributionStats key =
        case HM.lookup key sample of
            Nothing ->
                Nothing

            (Just (Distribution stats)) ->
                Just stats

            (Just _) ->
                Nothing

    errVal = (-1)
