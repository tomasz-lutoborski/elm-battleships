module Main exposing (main)

import Browser exposing (document)
import Model exposing (Model, initialModel)
import Update exposing (Msg, update)
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
