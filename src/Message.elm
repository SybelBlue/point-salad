module Message exposing (..)
import Vector3 exposing (Index)
import Card exposing (Card)
import Veggie exposing (Veggie)
import Either exposing (Either(..))

type alias Selection =
    { aisle : Index
    , item : Either Card { veggie : Veggie, first : Bool }
    }

type Msg 
    = Selected Selection

selectObjective : Index -> Card -> Msg
selectObjective i c = Selected { item = Left c, aisle = i }

