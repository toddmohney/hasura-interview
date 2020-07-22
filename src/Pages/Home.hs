module Pages.Home where

import           Control.Lens hiding (pre)
import           Data.Text (Text)
import           Prelude hiding (head, id, div, span)
import           Text.Blaze.Html5 hiding (title)
import           Text.Blaze.Html5.Attributes

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
            h1 "Server Health Metrics"
            p . toHtml $ page ^. message

            div ! class_ "row" $ do
                div ! class_ "col-lg-3" $ do
                    h2 "API endpoint metrics"
                    hr
                    pre
                        ! id "api-metrics"
                        $ toHtml ("" :: Text)

                div ! class_ "col-lg-3" $ do
                    h2 "API RAW endpoint metrics"
                    hr
                    pre
                        ! id "api-raw-metrics"
                        $ toHtml ("" :: Text)

                div ! class_ "col-lg-3" $ do
                    h2 "GC metrics"
                    hr
                    pre
                        ! id "gc-metrics"
                        $ toHtml ("" :: Text)
