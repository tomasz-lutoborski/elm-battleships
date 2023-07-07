module Update exposing (Msg(..), update)

import Model exposing (Board, CurrentPlayer(..), Game, Mode, Model, Orientation(..), Position, Ship, State(..), Target, Variant, initialGame)
import Process
import Ships exposing (carrier)
import Task


type Msg
    = ChooseMode Mode
    | ChooseVariant Variant
    | Shoot Int Int
    | ChangeState State
    | TargetHit Target
    | RotateShip
    | PlaceShip Position
    | ChoosePlacedShip Ship
    | ChangeCurrentPlayer
    | SetFlashMessage String
    | ClearFlashMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCurrentPlayer ->
            let
                currentPlayer =
                    model.game.currentPlayer

                nextPlayer =
                    case currentPlayer of
                        Player1 ->
                            Player2

                        Player2 ->
                            Player1

                currentGame =
                    model.game
            in
            ( { model | game = { currentGame | currentPlayer = nextPlayer } }, Cmd.none )

        ChooseMode mode ->
            ( { model | mode = mode }, Cmd.none )

        ChangeState state ->
            case state of
                PlacingShips ->
                    ( { model | game = initialGame, state = state, placedShip = Just (carrier Nothing Horizontal) }, Cmd.none )

                Menu ->
                    ( { model | state = state }, Cmd.none )

                Playing ->
                    case validBoards model of
                        True ->
                            ( { model | state = state }, Cmd.none )

                        False ->
                            ( model, Cmd.none )

        Shoot x y ->
            Debug.todo "MakeMove"

        ChooseVariant variant ->
            ( { model | variant = variant }, Cmd.none )

        TargetHit target ->
            Debug.todo "TargetHit"

        ChoosePlacedShip ship ->
            ( { model | placedShip = Just ship }, Cmd.none )

        RotateShip ->
            case model.placedShip of
                Just placedShip ->
                    case placedShip.orientation of
                        Horizontal ->
                            ( { model | placedShip = Just { placedShip | orientation = Vertical } }, Cmd.none )

                        Vertical ->
                            ( { model | placedShip = Just { placedShip | orientation = Horizontal } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PlaceShip pos ->
            case model.placedShip of
                Just placedShip ->
                    let
                        updatedShip =
                            { placedShip | position = Just pos }

                        currentPlayer =
                            model.game.currentPlayer

                        currentBoard =
                            case currentPlayer of
                                Player1 ->
                                    model.game.player1Board

                                Player2 ->
                                    model.game.player2Board

                        currentPlacedShips =
                            case currentPlayer of
                                Player1 ->
                                    model.game.placedShips1

                                Player2 ->
                                    model.game.placedShips2

                        updatedBoard =
                            placeShipOnBoard currentPlacedShips updatedShip currentBoard

                        updateGame : Board -> Ship -> Game -> Game
                        updateGame board ship game =
                            case game.currentPlayer of
                                Player1 ->
                                    { game | player1Board = board, placedShips1 = ship :: game.placedShips1 }

                                Player2 ->
                                    { game | player2Board = board, placedShips2 = ship :: game.placedShips2 }
                    in
                    case updatedBoard of
                        Just newBoard ->
                            ( { model | placedShip = Just updatedShip, game = updateGame newBoard updatedShip model.game }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetFlashMessage message ->
            ( { model | flashMessage = Just message }
            , Process.sleep (3 * 1000) |> Task.perform (always ClearFlashMessage)
            )

        ClearFlashMessage ->
            ( { model | flashMessage = Nothing }, Cmd.none )


validBoards : Model -> Bool
validBoards model =
    if List.length model.game.placedShips1 == 5 && List.length model.game.placedShips2 == 5 then
        True

    else
        False


placeShipOnBoard : List Ship -> Ship -> Board -> Maybe Board
placeShipOnBoard placedShips ship board =
    case ship.position of
        Just ( x, y ) ->
            if isValidShipPosition ship board && not (List.any (\s -> s.name == ship.name) placedShips) then
                Just <|
                    List.indexedMap
                        (\rowIndex row ->
                            List.indexedMap
                                (\colIndex cell ->
                                    if rowIndex == y && colIndex >= x && colIndex < x + ship.size && ship.orientation == Horizontal then
                                        { cell | isShip = True }

                                    else if colIndex == x && rowIndex >= y && rowIndex < y + ship.size && ship.orientation == Vertical then
                                        { cell | isShip = True }

                                    else
                                        cell
                                )
                                row
                        )
                        board

            else
                Nothing

        Nothing ->
            Just board


isValidShipPosition : Ship -> Board -> Bool
isValidShipPosition ship board =
    let
        boardWidth =
            List.length (List.head board |> Maybe.withDefault [])

        boardHeight =
            List.length board

        size =
            ship.size
    in
    case ship.position of
        Just ( x, y ) ->
            case ship.orientation of
                Horizontal ->
                    x >= 0 && y >= 0 && x + size <= boardWidth && y < boardHeight

                Vertical ->
                    x >= 0 && y >= 0 && x < boardWidth && y + ship.size <= boardHeight

        Nothing ->
            False
