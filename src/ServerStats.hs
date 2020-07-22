module ServerStats where

import           Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
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


data RequestMetrics = RequestMetrics
    { inFlightRequests :: Int64
    , responsesStats :: ResponseCount
    }

$(deriveJSON defaultOptions ''RequestMetrics)


data GCMetrics = GCMetrics
    { parMaxBytesCopied :: Int64
    , numGCs :: Int64
    , mutatorWallTime :: Int64
    , gcCPUTime :: Int64
    , parAvgBytesCopied :: Int64
    , initCPUTime :: Int64
    , bytesAllocated :: Int64
    , numBytesUsageSamples :: Int64
    , currentBytesSlop :: Int64
    , maxBytesSlop :: Int64
    , parTotalBytesCopied :: Int64
    , cumulativeBytesUsed :: Int64
    , gcWallTime :: Int64
    , mutatorCPUTime :: Int64
    , peakMBAllocated :: Int64
    , initWallTime :: Int64
    , maxBytesUsed :: Int64
    , bytesCopied :: Int64
    , wallTime :: Int64
    , currentBytesUsed :: Int64
    , cpuTime :: Int64
    }

$(deriveJSON defaultOptions ''GCMetrics)


data ServerStats = ServerStats
    { requestMetrics :: RequestMetrics
    , rawRequestMetrics :: RequestMetrics
    , gcMetrics :: GCMetrics
    }

$(deriveJSON defaultOptions ''ServerStats)


fromValue
    :: Sample
    -> ServerStats
fromValue sample = ServerStats
    { requestMetrics = mkRequestMetrics sample
    , rawRequestMetrics = mkRawRequestMetrics sample
    , gcMetrics = mkGCMetrics sample
    }


mkRequestMetrics
    :: Sample
    -> RequestMetrics
mkRequestMetrics sample = RequestMetrics
    { inFlightRequests = getGaugeValue "servant.path.GET.in_flight" sample
    , responsesStats = ResponseCount
        { totalResponseCount = getCounterValue "servant.path.GET.responses.XXX" sample
        , responseStatusCounts = ResponseStatusCount
            { fiveHundreds = getCounterValue "servant.path.GET.responses.5XX" sample
            , fourHundreds = getCounterValue "servant.path.GET.responses.4XX" sample
            , twoHundreds = getCounterValue "servant.path.GET.responses.2XX" sample
            }
        , responseTimeDistribution = getDistributionStats "servant.path.GET.time_ms" sample
        }
    }


mkRawRequestMetrics
    :: Sample
    -> RequestMetrics
mkRawRequestMetrics sample = RequestMetrics
    { inFlightRequests = getGaugeValue "servant.path.public.RAW.in_flight" sample
    , responsesStats = ResponseCount
        { totalResponseCount = getCounterValue "servant.path.public.RAW.responses.XXX" sample
        , responseStatusCounts = ResponseStatusCount
            { fiveHundreds = getCounterValue "servant.path.public.RAW.responses.5XX" sample
            , fourHundreds = getCounterValue "servant.path.public.RAW.responses.4XX" sample
            , twoHundreds = getCounterValue "servant.path.public.RAW.responses.2XX" sample
            }
        , responseTimeDistribution = getDistributionStats "servant.path.public.RAW.time_ms" sample
        }
    }


mkGCMetrics
    :: Sample
    -> GCMetrics
mkGCMetrics sample = GCMetrics
    { parMaxBytesCopied = getGaugeValue "rts.gc.par_max_bytes_copied" sample
    , numGCs = getCounterValue "rts.gc.num_gcs" sample
    , mutatorWallTime = getCounterValue "rts.gc.mutator_wall_ms" sample
    , gcCPUTime = getCounterValue "rts.gc.gc_cpu_ms" sample
    , parAvgBytesCopied = getGaugeValue "rts.gc.par_avg_bytes_copied" sample
    , initCPUTime = getCounterValue "rts.gc.init_cpu_ms" sample
    , bytesAllocated = getCounterValue "rts.gc.bytes_allocated" sample
    , numBytesUsageSamples = getCounterValue "rts.gc.num_bytes_usage_samples" sample
    , currentBytesSlop = getGaugeValue "rts.gc.current_bytes_slop" sample
    , maxBytesSlop = getGaugeValue "rts.gc.max_bytes_slop" sample
    , parTotalBytesCopied = getGaugeValue "rts.gc.par_tot_bytes_copied" sample
    , cumulativeBytesUsed = getCounterValue "rts.gc.cumulative_bytes_used" sample
    , gcWallTime = getCounterValue "rts.gc.gc_wall_ms" sample
    , mutatorCPUTime = getCounterValue "rts.gc.mutator_cpu_ms" sample
    , peakMBAllocated = getGaugeValue "rts.gc.peak_megabytes_allocated" sample
    , initWallTime = getCounterValue "rts.gc.init_wall_ms" sample
    , maxBytesUsed = getGaugeValue "rts.gc.max_bytes_used" sample
    , bytesCopied = getCounterValue "rts.gc.bytes_copied" sample
    , wallTime = getCounterValue "rts.gc.wall_ms" sample
    , currentBytesUsed = getGaugeValue "rts.gc.current_bytes_used" sample
    , cpuTime = getCounterValue "rts.gc.cpu_ms" sample
    }


getGaugeValue
    :: Text
    -> Sample
    -> Int64
getGaugeValue key sample =
    case HM.lookup key sample of
        Nothing ->
            errVal

        (Just (Gauge val)) ->
            val

        (Just _) ->
            errVal


getCounterValue
    :: Text
    -> Sample
    -> Int64
getCounterValue key sample =
    case HM.lookup key sample of
        Nothing ->
            errVal

        (Just (Counter val)) ->
            val

        (Just _) ->
            errVal


getDistributionStats
    :: Text
    -> Sample
    -> Maybe Stats
getDistributionStats key sample =
    case HM.lookup key sample of
        Nothing ->
            Nothing

        (Just (Distribution stats)) ->
            Just stats

        (Just _) ->
            Nothing


errVal :: Int64
errVal = (-1)
