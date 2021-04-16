module Cards exposing (cards)

import Data exposing (Veggie(..), Objective(..), Card(..), VeggieDict, Points)
import VeggieDict

cycle3 : (a -> b -> c -> d) -> (b -> c -> a -> d)
cycle3 fabcd b c a = fabcd a b c

insert : Veggie -> a -> VeggieDict a -> VeggieDict a
insert = cycle3 VeggieDict.insert

one : Veggie -> Points -> Objective
one v0 p0 = Items <| insert v0 p0 VeggieDict.empty

two : Veggie -> Points -> Veggie -> Points -> Objective
two v0 p0 v1 p1 = Items <| insert v1 p1 <| insert v0 p0 VeggieDict.empty

three : Veggie -> Points -> Veggie -> Points -> Veggie -> Points -> Objective
three v0 p0 v1 p1 v2 p2 = Items <| insert v2 p2 <| insert v1 p1 <| insert v0 p0 VeggieDict.empty

card : Veggie -> Objective -> Int -> Card
card = cycle3 Card

{-| Full deck of cards for the game -}
cards : List Card
cards = List.indexedMap (\i f -> f i) 
    []