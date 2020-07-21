module RouteHandlers.Home where

import           Pages.Home (HomePage(..))


home
    :: ( Monad m
       )
    => m HomePage
home =
    pure HomePage
        { _message = "Hiya!"
        }
