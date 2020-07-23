module RouteHandlers.Home where

import qualified Data.Text as T

import           Interview.Class.FastLogger (FastLogger(..))
import           Interview.Class.Instrumentation (Instrumentation(..))
import           Pages.Home (HomePage(..), HelloPage(..))


home
    :: ( FastLogger m
       , Instrumentation m
       )
    => m HomePage
home = do
    sampleAll
        >>= logInfo . T.pack . show

    pure HomePage


hello
    :: ( Applicative m
       )
    => m HelloPage
hello = do
    pure HelloPage
