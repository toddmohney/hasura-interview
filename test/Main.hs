module Main where

import           Control.Monad (unless)
import qualified Hedgehog as H
import qualified Hedgehog.Classes as HC
import qualified Spec
import           System.Exit (exitFailure)
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

import qualified Test.Interview.ServerStatsSpec as ServerStatsSpec

main :: IO ()
main = do
    putStrLn "Running property tests..."
    propertyTestsPassed <- H.checkParallel
        ServerStatsSpec.propertyTests


    putStrLn "Running lawful tests..."
    lawfulTestsPassed <- HC.lawsCheckMany
        ServerStatsSpec.lawfulTests


    putStrLn "Running spec tests..."
    hspecWith defaultConfig {configFormatter = Just specdoc} Spec.spec

    unless (propertyTestsPassed && lawfulTestsPassed) exitFailure

