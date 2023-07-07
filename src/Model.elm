module Model exposing (Board, Cell, CurrentPlayer(..), Game, Mode(..), Model, Orientation(..), Position, Ship, State(..), Target, Variant(..), initialGame, initialModel)


type alias Model =
    { mode : Mode
    , variant : Variant
    , game : Game
    , state : State
    , placedShip : Maybe Ship
    , flashMessage : Maybe String
    }


type Variant
    = Standard
    | Salvo
    | Mines


type State
    = PlacingShips
    | Menu
    | Playing


type alias Game =
    { player1Board : Board
    , player2Board : Board
    , placedShips1 : List Ship
    , placedShips2 : List Ship
    , currentPlayer : CurrentPlayer
    }


type CurrentPlayer
    = Player1
    | Player2


type Mode
    = PlayerVsPlayer
    | PlayerVsComputer


type alias Board =
    List (List Cell)


type alias Cell =
    { isShip : Bool
    , isHit : Bool
    }


type Orientation
    = Horizontal
    | Vertical


type alias Position =
    ( Int, Int )


type alias Ship =
    { size : Int
    , orientation : Orientation
    , position : Maybe Position
    , name : String
    }


type Target
    = Hit
    | Miss
    | Sunk


initialModel : Model
initialModel =
    { mode = PlayerVsPlayer
    , variant = Standard
    , game = initialGame
    , state = Menu
    , placedShip = Nothing
    , flashMessage = Nothing
    }


initialGame : Game
initialGame =
    { player1Board = emptyBoard
    , player2Board = emptyBoard
    , currentPlayer = Player1
    , placedShips1 = []
    , placedShips2 = []
    }


emptyBoard : Board
emptyBoard =
    List.repeat 10 (List.repeat 10 { isShip = False, isHit = False })
