module Message exposing (..)
import Vector3 exposing (Index)
import Card exposing (Card)
import Veggie exposing (Veggie)

type alias Selection =
    { aisle : Index
    , item : Result Card { veggie : Veggie, first : Bool }
    }

type Msg 
    = Selected Selection

selectObjective : Index -> Card -> Msg
selectObjective i c = Selected { item = Err c, aisle = i }

selectVeggie : Index -> Veggie -> Bool -> Msg
selectVeggie i v first = Selected { item = Ok { veggie = v, first = first }, aisle = i }
