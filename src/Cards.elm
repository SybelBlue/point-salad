module Cards exposing (cards)

import Data exposing (Veggie(..), Objective(..), Card, VeggieDict, Points)
import VeggieDict

insert : Veggie -> a -> VeggieDict a -> VeggieDict a
insert k v d = VeggieDict.insert d k v

one : Veggie -> Points -> Objective
one v0 p0 = Items <| insert v0 p0 VeggieDict.empty

two : Veggie -> Points -> Veggie -> Points -> Objective
two v0 p0 v1 p1 = Items <| insert v1 p1 <| insert v0 p0 VeggieDict.empty

three : Veggie -> Points -> Veggie -> Points -> Veggie -> Points -> Objective
three v0 p0 v1 p1 v2 p2 = Items <| insert v2 p2 <| insert v1 p1 <| insert v0 p0 VeggieDict.empty

{-| Full deck of cards for the game -}
cards : List Card
cards = Debug.todo "make meee"