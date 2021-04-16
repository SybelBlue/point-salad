module VeggieDict exposing (..)

import Data exposing (VeggieDict, Veggie(..))

{-| Returns a new, empty VeggieDict -}
empty : VeggieDict a
empty = 
    { tomato = Nothing
    , carrot = Nothing
    , pepper = Nothing
    , lettuce = Nothing
    , cabbage = Nothing
    , onion = Nothing
    }

{-| Inserts a value into a VeggieDict using a Veggie key, 
    replacing what was there before 
 -}
insert : VeggieDict a -> Veggie -> a -> VeggieDict a
insert vd k v = 
    case k of
        Tomato -> { vd | tomato = Just v }
        Carrot -> { vd | carrot = Just v }
        Pepper -> { vd | pepper = Just v }
        Lettuce -> { vd | lettuce = Just v }
        Cabbage -> { vd | cabbage = Just v }
        Onion -> { vd | onion = Just v }

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
