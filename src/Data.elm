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
    = Combo (List Veggie) Points   -- always len 2, 3, or 6
    | Stacked Veggie Int Points  -- always _ 2 5 or _ 3 8
    | Items (VeggieDict Points) -- always len 1, 2, or 3
    | Most Veggie Points  -- always _ 10
    | Fewest Veggie Points  -- always _ 7
    | MostTotal Points  -- always 10
    | FewestTotal Points  -- always 7
    | PerTypeWith Int Points  -- always 2 3 or 3 5
    | PerMissing Points  -- always 5
    | EvenOdd Veggie Points Points  -- always _ 7 3


type Card = Card Int Veggie Objective
