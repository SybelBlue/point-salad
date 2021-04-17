module Model exposing (..)
import Game exposing (GameBody, PlayerId)
import Draw exposing (Draw, Seed)
import Card exposing (Card)
import SideEffect exposing (..)
import Basics.Extra exposing (uncurry)

{-| (# of Players, Startseed) -}
type alias Flags = (Int, PlayerId, Seed)

type alias Model =
    { body : GameBody
    , seData : (List Card, Seed)
    }

type alias GameAction a = SE Model a

intoAction : Draw a -> GameAction a
intoAction da = SE (\model -> 
        let
            (c, nse) = run da model.seData
        in
            (c, { model | seData = nse }))

type alias ModelUpdate a = a -> Model -> Model

updateAction : ModelUpdate a -> ModelUpdate (GameAction a)
updateAction fam ga = uncurry fam << run ga

draw : GameAction Card
draw = intoAction Draw.card

init : Flags -> (Model, Cmd msg)
init _ = 
    let
        test = 3
    in
        (Debug.todo "write meeee", Cmd.none)
