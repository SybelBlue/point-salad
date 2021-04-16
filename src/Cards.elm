module Cards exposing (cards)

import Card exposing (Veggie(..), Objective(..), Card(..), VeggieDict, Points)
import Veggie

cycle3 : (a -> b -> c -> d) -> (b -> c -> a -> d)
cycle3 fabcd b c a = fabcd a b c

insert : Veggie -> a -> VeggieDict a -> VeggieDict a
insert = cycle3 Veggie.insert

one : Veggie -> Points -> Objective
one v0 p0 = Items <| insert v0 p0 Veggie.empty

two : Veggie -> Points -> Veggie -> Points -> Objective
two v0 p0 v1 p1 = Items <| insert v1 p1 <| insert v0 p0 Veggie.empty

three : Veggie -> Points -> Veggie -> Points -> Veggie -> Points -> Objective
three v0 p0 v1 p1 v2 p2 = Items <| insert v2 p2 <| insert v1 p1 <| insert v0 p0 Veggie.empty

card : Veggie -> Objective -> Int -> Card
card = cycle3 Card

{-| Full deck of cards for the game -}
cards : List Card
cards = List.indexedMap (\i f -> f i) 
    [ card Tomato <| two Onion 1 Cabbage 1
    , card Tomato <| EvenOdd Onion 7 3 
    , card Tomato <| Combo [Tomato, Onion, Lettuce] 8
    , card Tomato <| three Onion 4 Carrot -2 Lettuce -2
    , card Tomato <| Most Onion 10
    , card Tomato <| Fewest Onion 7
    , card Tomato <| Combo [Carrot, Onion, Pepper] 8
    , card Tomato <| three Onion 2 Pepper 2 Cabbage -4
    , card Tomato <| Stacked Onion 2 5
    , card Tomato <| three Onion 2 Cabbage 1 Tomato -2
    , card Tomato <| Stacked Onion 3 8
    , card Tomato <| two Onion 3 Pepper -2
    , card Tomato <| one Onion 2
    , card Tomato <| Combo [Tomato, Carrot, Pepper, Lettuce, Cabbage, Onion] 12
    , card Tomato <| Combo [Cabbage, Pepper] 5
    , card Tomato <| three Onion 3 Cabbage -1 Tomato -1
    , card Tomato <| two Onion 1 Carrot 1
    , card Tomato <| Combo [Carrot, Lettuce] 5
    , card Carrot <| PerTypeWith 3 5
    , card Carrot <| Most Cabbage 10
    , card Carrot <| two Cabbage 1 Lettuce 1
    , card Carrot <| Stacked Cabbage 2 5
    , card Carrot <| Combo [Onion, Pepper] 5
    , card Carrot <| three Cabbage 2 Tomato 2 Lettuce -4
    , card Carrot <| Fewest Cabbage 7
    , card Carrot <| three Cabbage 3 Lettuce -1 Carrot -1
    , card Carrot <| three Cabbage 2 Lettuce 1 Carrot -2
    , card Carrot <| two Cabbage 1 Pepper 1
    , card Carrot <| EvenOdd Cabbage 7 3
    , card Carrot <| two Cabbage 3 Tomato -2
    , card Carrot <| Stacked Cabbage 3 8
    , card Carrot <| three Cabbage 4 Pepper -2 Onion -2
    , card Carrot <| Combo [Carrot, Cabbage, Onion] 8
    , card Carrot <| Combo [Tomato, Cabbage, Pepper] 8
    , card Carrot <| one Cabbage 2
    , card Carrot <| Combo [Tomato, Lettuce] 5
    , card Pepper <| Fewest Lettuce 7
    , card Pepper <| two Lettuce 3 Carrot -2
    , card Pepper <| Combo [Cabbage, Tomato] 5
    , card Pepper <| two Lettuce 1 Onion 1
    , card Pepper <| two Lettuce 1 Tomato 1
    , card Pepper <| Stacked Lettuce 3 8
    , card Pepper <| Combo [Tomato, Lettuce, Carrot] 8
    , card Pepper <| MostTotal 10
    , card Pepper <| Combo [Carrot, Onion] 5
    , card Pepper <| three Lettuce 4 Tomato -2 Cabbage -2
    , card Pepper <| EvenOdd Lettuce 7 3
    , card Pepper <| three Lettuce 2 Onion 1 Pepper -2
    , card Pepper <| Stacked Lettuce 2 5
    , card Pepper <| three Lettuce 2 Carrot 2 Onion -4
    , card Pepper <| one Lettuce 2
    , card Pepper <| Combo [Pepper, Lettuce, Cabbage] 8
    , card Pepper <| three Lettuce 3 Onion -1 Pepper -1
    , card Pepper <| Most Lettuce 10
    , card Lettuce <| EvenOdd Pepper 7 3
    , card Lettuce <| FewestTotal 7
    , card Lettuce <| Most Pepper 10
    , card Lettuce <| Fewest Pepper 7
    , card Lettuce <| Combo [Carrot, Tomato] 5
    , card Lettuce <| two Pepper 1 Onion 1
    , card Lettuce <| two Pepper 3 Cabbage -2
    , card Lettuce <| Stacked Pepper 3 8
    , card Lettuce <| two Pepper 1 Tomato 1
    , card Lettuce <| one Pepper 2
    , card Lettuce <| Stacked Pepper 2 5
    , card Lettuce <| three Pepper 4 Onion -2 Carrot -2
    , card Lettuce <| Combo [Cabbage, Onion] 5
    , card Lettuce <| three Pepper 2 Tomato 1 Lettuce -2
    , card Lettuce <| Combo [Lettuce, Pepper, Carrot] 8
    , card Lettuce <| Combo [Onion, Pepper, Cabbage] 8
    , card Lettuce <| three Pepper 3 Tomato -1 Lettuce -1
    , card Lettuce <| three Pepper 2 Cabbage 2 Tomato -4
    , card Cabbage <| Combo [Tomato, Pepper] 5
    , card Cabbage <| Stacked Carrot 3 8
    , card Cabbage <| Fewest Carrot 7
    , card Cabbage <| one Carrot 2
    , card Cabbage <| three Carrot 3 Onion 2 Pepper -4
    , card Cabbage <| Combo [Lettuce, Carrot, Onion] 8
    , card Cabbage <| Combo [Cabbage, Carrot, Tomato] 8
    , card Cabbage <| three Carrot 4 Lettuce -2 Tomato -2
    , card Cabbage <| PerMissing 5
    , card Cabbage <| Most Carrot 10
    , card Cabbage <| three Carrot 2 Pepper 1 Cabbage -2
    , card Cabbage <| Stacked Carrot 2 5
    , card Cabbage <| two Carrot 3 Onion -2
    , card Cabbage <| Combo [Onion, Lettuce] 5
    , card Cabbage <| three Carrot 3 Pepper -1 Cabbage -1
    , card Cabbage <| two Carrot 1 Pepper 1
    , card Cabbage <| two Carrot 1 Lettuce 1
    , card Cabbage <| EvenOdd Carrot 7 3
    , card Onion <| Combo [Cabbage, Lettuce] 5
    , card Onion <| two Tomato 3 Lettuce -2
    , card Onion <| Combo [Cabbage, Tomato, Lettuce] 8
    , card Onion <| one Tomato 2
    , card Onion <| Stacked Tomato 3 8
    , card Onion <| Fewest Tomato 7
    , card Onion <| two Tomato 1 Carrot 1
    , card Onion <| PerTypeWith 2 3
    , card Onion <| Most Tomato 10
    , card Onion <| three Tomato 2 Lettuce 2 Carrot -4
    , card Onion <| Combo [Carrot, Pepper] 5
    , card Onion <| two Tomato 1 Cabbage 1
    , card Onion <| Combo [Onion, Tomato, Pepper] 8
    , card Onion <| three Tomato 3 Carrot -1 Onion -1
    , card Onion <| three Tomato 4 Cabbage -2 Pepper -2
    , card Onion <| Stacked Tomato 2 5
    , card Onion <| EvenOdd Tomato 7 3
    , card Onion <| three Tomato 2 Carrot 1 Onion -2
    ]