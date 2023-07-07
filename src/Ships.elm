module Ships exposing (battleship, carrier, cruiser, destroyer, submarine)

import Model exposing (Orientation, Position, Ship)


carrier : Maybe Position -> Orientation -> Ship
carrier position orientation =
    { size = 5
    , orientation = orientation
    , position = position
    , name = "Carrier"
    }


battleship : Maybe Position -> Orientation -> Ship
battleship position orientation =
    { size = 4
    , orientation = orientation
    , position = position
    , name = "Battleship"
    }


cruiser : Maybe Position -> Orientation -> Ship
cruiser position orientation =
    { size = 3
    , orientation = orientation
    , position = position
    , name = "Cruiser"
    }


submarine : Maybe Position -> Orientation -> Ship
submarine position orientation =
    { size = 3
    , orientation = orientation
    , position = position
    , name = "Submarine"
    }


destroyer : Maybe Position -> Orientation -> Ship
destroyer position orientation =
    { size = 2
    , orientation = orientation
    , position = position
    , name = "Destroyer"
    }
