module Data exposing (..)

type alias Points = Int

type Veggie
    = Tomato
    | Carrot
    | Pepper
    | Lettuce
    | Cabbage
    | Onion

type alias VeggieDict a = 
    { tomato : Maybe a
    , carrot : Maybe a
    , pepper : Maybe a
    , lettuce : Maybe a
    , cabbage : Maybe a
    , onion : Maybe a
    }

type Objective
    = Combo (List Veggie) Points
    | Items (VeggieDict Points)
    | Most Veggie Points
    | Fewest Veggie Points
    | FewestTotal Points
    | MostTotal Points
    | PerTypeWith Int Points
    | PerMissing Points
    | EvenOdd Veggie Points Points


type Card = Card Int Veggie Objective
