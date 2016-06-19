import Navigation exposing (makeParser)

import Init exposing (..)
import Update exposing (update)
import Subscription exposing (subs)
import Views exposing (view)

main =
    Navigation.program
        (makeParser urlHashParser)
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        , urlUpdate = urlUpdate
        }
