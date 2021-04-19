module Card exposing (..)

import Veggie exposing (..)

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


isGlobalObjective : Objective -> Bool
isGlobalObjective o =
    case o of
        Most _ _ ->
            True
        
        Fewest _ _ ->
            True
        
        MostTotal _ ->
            True
        
        FewestTotal _ -> 
            True

        _ ->
            False

type Card = Card Int Veggie Objective

id : Card -> Int
id (Card i _ _) = i

veggie : Card -> Veggie
veggie (Card _ v _) = v

objective : Card -> Objective
objective (Card _ _ o) = o
