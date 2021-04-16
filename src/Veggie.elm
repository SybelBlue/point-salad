module Veggie exposing (Veggie(..), VeggieDict, empty, get, insert, size)

{-| The basic vegetable types -}
type Veggie
    = Tomato
    | Carrot
    | Pepper
    | Lettuce
    | Cabbage
    | Onion

{-| A type to represent a mapping from Veggie to Value a -}
type VeggieDict a = VDict
    { tomato : Maybe a
    , carrot : Maybe a
    , pepper : Maybe a
    , lettuce : Maybe a
    , cabbage : Maybe a
    , onion : Maybe a
    , size : Int
    }

{-| Returns a new, empty VeggieDict -}
empty : VeggieDict a
empty = VDict
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
insert (VDict vd) k v = VDict <|
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
get (VDict vd) k = 
    case k of
        Tomato -> vd.tomato
        Carrot -> vd.carrot
        Pepper -> vd.pepper
        Lettuce -> vd.lettuce
        Cabbage -> vd.cabbage
        Onion -> vd.onion

{-| Returns the number of entries in this VeggieDict -}
size : VeggieDict a -> Int
size (VDict v) = v.size
