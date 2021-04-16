module Veggie exposing (..)

import Card exposing (VeggieDict, Veggie(..))

{-| Returns a new, empty VeggieDict -}
empty : VeggieDict a
empty = 
    { tomato = Nothing
    , carrot = Nothing
    , pepper = Nothing
    , lettuce = Nothing
    , cabbage = Nothing
    , onion = Nothing
    , size = 0
    }

{-| Inserts a value into a VeggieDict using a Veggie key, 
    replacing what was there before 
 -}
insert : VeggieDict a -> Veggie -> a -> VeggieDict a
insert vd k v = 
    case k of
        Tomato -> { vd | tomato = Just v, size = if vd.tomato == Nothing then vd.size + 1 else vd.size }
        Carrot -> { vd | carrot = Just v, size = if vd.carrot == Nothing then vd.size + 1 else vd.size }
        Pepper -> { vd | pepper = Just v, size = if vd.pepper == Nothing then vd.size + 1 else vd.size }
        Lettuce -> { vd | lettuce = Just v, size = if vd.lettuce == Nothing then vd.size + 1 else vd.size }
        Cabbage -> { vd | cabbage = Just v, size = if vd.cabbage == Nothing then vd.size + 1 else vd.size }
        Onion -> { vd | onion = Just v, size = if vd.onion == Nothing then vd.size + 1 else vd.size }

{-| Gets a value from a VeggieDict using a Veggie key if present,
    Nothing otherwise
 -}
get : VeggieDict a -> Veggie -> Maybe a
get vd k = 
    case k of
        Tomato -> vd.tomato
        Carrot -> vd.carrot
        Pepper -> vd.pepper
        Lettuce -> vd.lettuce
        Cabbage -> vd.cabbage
        Onion -> vd.onion
