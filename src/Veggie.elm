module Veggie exposing (Veggie(..), VeggieDict, empty, get, insert, size)

import Vector6 exposing (Index(..), Vector6)

{-| The basic vegetable types -}
type Veggie
    = Tomato
    | Carrot
    | Pepper
    | Lettuce
    | Cabbage
    | Onion

toIndex : Veggie -> Index
toIndex v =
    case v of
        Tomato -> Index0
        Carrot -> Index1
        Pepper -> Index2
        Lettuce -> Index3
        Cabbage -> Index4
        Onion -> Index5

{-| A type to represent a mapping from Veggie to Value a -}
type VeggieDict a = VDict
    { data : Vector6 (Maybe a)
    , size : Int
    }

{-| Returns a new, empty VeggieDict -}
empty : VeggieDict a
empty = VDict
    { data = Vector6.repeat Nothing
    , size = 0
    }

{-| Inserts a value into a VeggieDict using a Veggie key, 
    replacing what was there before 
 -}
insert : VeggieDict a -> Veggie -> a -> VeggieDict a
insert (VDict vd) k v =
    let
        i : Index
        i = toIndex k
        prev : Maybe a
        prev = Vector6.get i vd.data
    in VDict { vd | data = Vector6.set i (Just v) vd.data, size = if prev == Nothing then vd.size + 1 else vd.size }
    

{-| Gets a value from a VeggieDict using a Veggie key if present,
    Nothing otherwise
 -}
get : VeggieDict a -> Veggie -> Maybe a
get (VDict vd) k = Vector6.get (toIndex k) vd.data

{-| Returns the number of entries in this VeggieDict -}
size : VeggieDict a -> Int
size (VDict v) = v.size
