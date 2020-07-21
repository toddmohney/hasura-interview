module Pages.Home where

import           Control.Lens
import           Data.Text (Text)
import           Prelude hiding (head, id, div, span)
import           Text.Blaze.Html5 hiding (title)

import           Components.MainContainer (wrapperHtml)


data HomePage = HomePage
    { _message :: Text
    } deriving (Show)

makeLenses ''HomePage


instance ToMarkup HomePage where
    toMarkup = homePageHtml


homePageHtml
    :: HomePage
    -> Markup
homePageHtml page =
    wrapperHtml $
        div $ do
            p "Welcome!"
            p . toHtml $ page ^. message
