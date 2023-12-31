module View exposing (view, viewMenu)

import Browser exposing (Document)
import Html exposing (Html, button, div, h2, option, select, text)
import Html.Attributes exposing (class, classList, selected, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Board, Cell, CurrentPlayer(..), Mode(..), Model, Orientation(..), Position, Ship, State(..), Variant(..))
import Ships exposing (..)
import Update exposing (Msg(..))


viewMenu : Model -> Html Msg
viewMenu model =
    let
        inputHandlerVariant : String -> Msg
        inputHandlerVariant text =
            case text of
                "Standard" ->
                    ChooseVariant Standard

                "Salvo" ->
                    ChooseVariant Salvo

                "Mines" ->
                    ChooseVariant Mines

                _ ->
                    -- Handle unexpected values
                    ChooseVariant Standard

        inputHandlerMode : String -> Msg
        inputHandlerMode text =
            case text of
                "AI" ->
                    ChooseMode PlayerVsComputer

                "Player" ->
                    ChooseMode PlayerVsPlayer

                _ ->
                    ChooseMode PlayerVsComputer
    in
    div [ class "menu" ]
        [ select [ class "select select__main", onInput (Debug.log "selected" inputHandlerVariant) ]
            [ option [ value "Standard", selected True ] [ text "Standard" ]
            , option [ value "Salvo" ] [ text "Salvo" ]
            , option [ value "Mines" ] [ text "With Mines" ]
            ]
        , select [ class "select select__main", onInput inputHandlerMode ]
            [ option [ value "AI" ] [ text "AI" ]
            , option [ value "Player", selected True ] [ text "Player" ]
            ]
        , button [ class "button button--accept", onClick (ChangeState PlacingShips) ] [ text "Play" ]
        ]


chooseBoard : Model -> Board
chooseBoard model =
    case model.game.currentPlayer of
        Player1 ->
            model.game.player1Board

        Player2 ->
            model.game.player2Board


viewRow : ( Int, List Cell ) -> (Position -> Msg) -> List (Html Msg)
viewRow ( rowIndex, row ) action =
    List.indexedMap (\columnIndex cell -> viewCell rowIndex columnIndex cell action) row


viewCell : Int -> Int -> Cell -> (Position -> Msg) -> Html Msg
viewCell row column cell action =
    let
        pos =
            ( column, row )
    in
    button [ classList [ ( "cell--ship", cell.isShip ) ], class "cell", onClick (action pos) ] []


viewPlacement : Model -> Html Msg
viewPlacement model =
    let
        board : Board
        board =
            chooseBoard model

        shipFromString : String -> Ship
        shipFromString str =
            let
                defaultPosition =
                    Nothing

                defaultOrientation =
                    Horizontal
            in
            case str of
                "carrier" ->
                    carrier defaultPosition defaultOrientation

                "battleship" ->
                    battleship defaultPosition defaultOrientation

                "cruiser" ->
                    cruiser defaultPosition defaultOrientation

                "submarine" ->
                    submarine defaultPosition defaultOrientation

                "destroyer" ->
                    destroyer defaultPosition defaultOrientation

                _ ->
                    carrier defaultPosition defaultOrientation
    in
    div [ class "placement__container" ]
        [ div [ class "placement__settings" ]
            [ button [ class "button button--warn", onClick (ChangeState Menu) ] [ text "Back to menu" ]
            , select [ class "select", onInput (ChoosePlacedShip << shipFromString) ]
                [ option [ value "carrier" ] [ text "Carrier" ]
                , option [ value "battleship" ] [ text "Battleship" ]
                , option [ value "cruiser" ] [ text "Cruiser" ]
                , option [ value "submarine" ] [ text "Submarine" ]
                , option [ value "destroyer" ] [ text "Destroyer" ]
                ]
            , button [ class "button button--util", onClick RotateShip ]
                [ case model.placedShip of
                    Nothing ->
                        text "Vertical"

                    Just ship ->
                        text
                            (if ship.orientation == Horizontal then
                                "To vertical"

                             else
                                "To horizontal"
                            )
                ]
            ]
        , div [ class "board" ]
            (List.concatMap (\row -> viewRow row PlaceShip) (List.indexedMap Tuple.pair board))
        , if model.mode == PlayerVsComputer || model.game.currentPlayer == Player2 then
            button [ class "button button--accept", onClick (ChangeState Playing) ] [ text "Ready to play" ]

          else
            button [ class "button button--accept", onClick ChangeCurrentPlayer ] [ text "Next player" ]
        ]


viewPlaying : Model -> Html Msg
viewPlaying model =
    let
        board : Board
        board =
            chooseBoard model
    in
    div [ class "placement__container" ]
        [ div [ class "placement__settings" ]
            [ button [ class "button button--warn", onClick (ChangeState Menu) ] [ text "Back to menu" ]
            ]
        , div [ class "board" ]
            (List.concatMap (\row -> viewRow row Shoot) (List.indexedMap Tuple.pair board))
        ]


view : Model -> Document Msg
view model =
    case model.state of
        Menu ->
            { title = "Battleships - Menu"
            , body =
                [ div []
                    [ case model.flashMessage of
                        Just message ->
                            div [ class "flash__container" ] [ h2 [ class "flash__text" ] [ text message ] ]

                        Nothing ->
                            text ""
                    , viewMenu model
                    ]
                ]
            }

        PlacingShips ->
            { title = "Battleships - Placing Ships"
            , body =
                [ div []
                    [ case model.flashMessage of
                        Just message ->
                            div [ class "flash__container" ] [ h2 [ class "flash__text" ] [ text message ] ]

                        Nothing ->
                            text ""
                    , viewPlacement model
                    ]
                ]
            }

        Playing ->
            { title = "Battleships - Playing"
            , body =
                [ div []
                    [ case model.flashMessage of
                        Just message ->
                            div [ class "flash__container" ] [ h2 [ class "flash__text" ] [ text message ] ]

                        Nothing ->
                            text ""
                    , viewPlaying model
                    ]
                ]
            }
