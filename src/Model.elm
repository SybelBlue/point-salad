module Model exposing (..)

import SideEffect exposing (..)
import Game exposing (GameBody, PlayerId)
import Draw exposing (Draw, Seed, gameBody)
import Card exposing (Card)
import Basics.Extra exposing (uncurry)
import Utils exposing (withNone)
import Cards exposing (cards)
import Message exposing (Msg)
import Flags exposing (Flags)

type alias Model =
    { body : GameBody
    , seData : (List Card, Seed)
    , pid : PlayerId
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

init : Flags -> (Model, Cmd Msg)
init flags = 
    let
        (body, seData) = run (gameBody flags.playerCount) (cards, flags.seed)
    in
        withNone { body = body, seData = seData, pid = flags.playerId }
