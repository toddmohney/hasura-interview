module Test.Interview.ServerStatsSpec
    ( main
    , spec
    , propertyTests
    , lawfulTests
    ) where

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.HashMap.Strict as HM
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Classes as HC
import           System.Metrics (Sample, Value(..))
import qualified System.Metrics.Distribution
import           Test.Hspec

import Interview.ServerStats


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "getGaugeValue" $ do
        context "when the key exists and the stat type is correct" $ do
            it "returns a useful value" $ do
                let key = "some-cool-stat"
                let gaugeValue = Gauge 55
                let testSample = HM.singleton key gaugeValue

                getGaugeValue key testSample
                    `shouldBe` 55


        context "when the key exists but the stat type is incorrect" $ do
            it "returns an error value" $ do
                let key = "some-cool-stat"
                let counterValue = Counter 55
                let testSample = HM.singleton key counterValue

                getGaugeValue key testSample
                    `shouldBe` -1


        context "when the key does not exists" $ do
            it "returns an error value" $ do
                let key = "some-cool-stat"
                let gaugeValue = Gauge 55
                let testSample = HM.singleton key gaugeValue

                getGaugeValue "some-missing-key" testSample
                    `shouldBe` -1


    describe "getCounterValue" $ do
        context "when the key exists and the stat type is correct" $ do
            it "returns a useful value" $ do
                let key = "some-cool-stat"
                let counterValue = Counter 55
                let testSample = HM.singleton key counterValue

                getCounterValue key testSample
                    `shouldBe` 55


        context "when the key exists but the stat type is incorrect" $ do
            it "returns an error value" $ do
                let key = "some-cool-stat"
                let gaugeValue = Gauge 55
                let testSample = HM.singleton key gaugeValue

                getCounterValue key testSample
                    `shouldBe` -1


        context "when the key does not exists" $ do
            it "returns an error value" $ do
                let key = "some-cool-stat"
                let counterValue = Counter 55
                let testSample = HM.singleton key counterValue

                getCounterValue "some-missing-key" testSample
                    `shouldBe` -1


    describe "getDistributionStats" $ do
        context "when the key exists and the stat type is correct" $ do
            it "returns a useful value" $ do
                let key = "some-cool-stat"
                distributionValue <- System.Metrics.Distribution.new
                stats <- System.Metrics.Distribution.read distributionValue
                let testSample = HM.singleton key (Distribution stats)

                getDistributionStats key testSample
                    `shouldBe` (Just stats)


        context "when the key exists but the stat type is incorrect" $ do
            it "returns Nothing" $ do
                let key = "some-cool-stat"
                let gaugeValue = Gauge 55
                let testSample = HM.singleton key gaugeValue

                getDistributionStats key testSample
                    `shouldBe` Nothing


        context "when the key does not exists" $ do
            it "returns Nothing" $ do
                let key = "some-cool-stat"
                distributionValue <- System.Metrics.Distribution.new
                stats <- System.Metrics.Distribution.read distributionValue
                let testSample = HM.singleton key (Distribution stats)

                getDistributionStats "some-missing-key" testSample
                    `shouldBe` Nothing


propertyTests :: H.Group
propertyTests =
    H.Group "ServerStatsSpec" []


lawfulTests :: [(String, [HC.Laws])]
lawfulTests =
  [ ("ResponseStatusCount", eqAndJSONLaws genResponseStatusCount)
  , ("GCMetrics", eqAndJSONLaws genGCMetrics)
  , ("ServerStats", eqAndJSONLaws genServerStats)
  ]


eqAndJSONLaws
    :: (Eq a, ToJSON a, FromJSON a, Show a)
    => H.Gen a
    -> [HC.Laws]
eqAndJSONLaws p =
    [ HC.eqLaws p
    , HC.jsonLaws p
    ]


genServerStats :: H.Gen ServerStats
genServerStats =
    ServerStats
        <$> genRequestMetrics
        <*> genRequestMetrics
        <*> genGCMetrics


genRequestMetrics :: H.Gen RequestMetrics
genRequestMetrics =
    RequestMetrics
        <$> Gen.int64 (Range.linear 0 1000000)
        <*> genResponseCount


genResponseCount :: H.Gen ResponseCount
genResponseCount =
    ResponseCount
        <$> Gen.int64 (Range.linear 0 1000000)
        <*> genResponseStatusCount
        <*> pure Nothing


genResponseStatusCount :: H.Gen ResponseStatusCount
genResponseStatusCount =
    ResponseStatusCount
        <$> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)


genGCMetrics :: H.Gen GCMetrics
genGCMetrics =
    GCMetrics
        <$> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
        <*> Gen.int64 (Range.linear 0 1000000)
