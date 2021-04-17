module Model exposing (..)
import Game exposing (GameBody)
import Draw exposing (Seed)
import Card exposing (Card)
import SideEffect exposing (..)

type alias Model =
    { body : GameBody
    , seData : (List Card, Seed)
    }

type alias GameAction a = SE Model a

draw : GameAction Card
draw = SE (\model -> 
        let
            (c, nse) = run Draw.card model.seData
        in
            (c, { model | seData = nse }))

init : Model
init = Debug.todo "write meeee"
