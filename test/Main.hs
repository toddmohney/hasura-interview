module Main where

import           Control.Monad (unless)
import qualified Spec
import           System.Exit (exitFailure)
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = do
    putStrLn "Running property tests..."
    results <- sequence
        []

    putStrLn ""

    putStrLn "Running spec tests..."
    hspecWith defaultConfig {configFormatter = Just specdoc} Spec.spec

    unless (and results) exitFailure

