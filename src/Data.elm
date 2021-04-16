module Data exposing (..)
import Html exposing (a)

type alias Points = Int

type Veggie
    = Tomato
    | Carrot
    | Pepper
    | Lettuce
    | Cabbage
    | Onion

type VeggieDict a = VDict
    { tomato : Maybe a
    , carrot : Maybe a
    , pepper : Maybe a
    , lettuce : Maybe a
    , cabbage : Maybe a
    , onion : Maybe a
    , size : Int
    }

type Objective
    = Set (List Veggie) Points
    | Items (VeggieDict Points)
    | Most Veggie Points
    | Least Veggie Points
    | FewestTotal Points
    | PerTypeWith Int Points
    | PerMissing Points
    | EvenOdd Veggie Points Points


type Card = Card Int Veggie Objective
