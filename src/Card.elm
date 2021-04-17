module Card exposing (..)

import Veggie exposing (..)
import Vector3 exposing (Vector3)

type alias Points = Int

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

type alias Aisle = Vector3 Card
