module Interview.Class.Instrumentation where

import System.Metrics (Sample)

class (Monad m) => Instrumentation m where
    sampleAll :: m Sample
