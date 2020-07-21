module Components.MainContainer where

import           Prelude hiding (head, id, div, span)
import           Text.Blaze.Html5 hiding (title)
import qualified Text.Blaze.Html5 as B
import           Text.Blaze.Html5.Attributes hiding (title, span)


wrapperHtml
    :: Markup
    -> Markup
wrapperHtml pageContent =
    html $ do
        head $ do
            B.title "Hello Everybody!"

            script
                ! src "https://code.jquery.com/jquery-3.5.1.slim.min.js"
                ! customAttribute "integrity" "sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj"
                ! customAttribute "crossorigin" "anonymous"
                $ span ""

            script
                ! src "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"
                ! customAttribute "integrity" "sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo"
                ! customAttribute "crossorigin" "anonymous"
                $ span ""

            link
                ! rel "stylesheet"
                ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css"
                ! customAttribute "integrity" "sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk"
                ! customAttribute "crossorigin" "anonymous"

            script
                ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.5.0/js/bootstrap.min.js"
                ! customAttribute "integrity" "sha384-OgVRvuATP1z7JjHLkuOU7Xw704+h835Lr+6QL9UvYjZE3Ipu6Tp75j7Bh/kR0JKI"
                ! customAttribute "crossorigin" "anonymous"
                $ span ""

        body $ do
            div ! class_ "container-fluid p-0" $ do
                mainNavHtml

                div ! class_ "container my-5" $
                    pageContent


mainNavHtml :: Markup
mainNavHtml =
    nav ! class_ "navbar navbar-dark bg-dark" $ do
        a
            ! class_ "navbar-brand"
            ! href "/"
            $ do
                img
                    ! src "/public/images/donkeyboat.jpg"
                    ! class_ "mr-2"
                    ! height (toValue (24 :: Int))
                    ! width (toValue (24 :: Int))

                span "Hasura Interview"
