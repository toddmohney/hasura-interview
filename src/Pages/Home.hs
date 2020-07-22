module Pages.Home where

import           Data.Text (Text)
import           Prelude hiding (head, id, div, span)
import           Text.Blaze.Html5 hiding (title)
import           Text.Blaze.Html5.Attributes

import           Components.MainContainer (wrapperHtml)


data HomePage = HomePage


instance ToMarkup HomePage where
    toMarkup = homePageHtml


homePageHtml
    :: HomePage
    -> Markup
homePageHtml _page =
    wrapperHtml $
        div $ do
            div ! class_ "jumbotron" $ do
                h1
                    ! class_ "display-4"
                    $ toHtml ("Server Health" :: Text)

                p
                    ! class_ "lead"
                    $ toHtml ("Useful metrics for observing the performance of your server." :: Text)


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
