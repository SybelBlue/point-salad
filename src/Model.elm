module Model exposing (..)

import SideEffect exposing (..)
import Game exposing (GameBody, PlayerId)
import Draw exposing (Draw, Seed, gameBody)
import Card exposing (Card)
import Utils exposing (withNone)
import Cards exposing (cards)
import Message exposing (Msg, Selection)
import Flags exposing (Flags)
import Tuple exposing (pair)
import Vector6

type alias Model =
    { body : GameBody
    , seData : (List Card, Seed)
    , pid : PlayerId
    , selected : Maybe Selection
    }

type alias GameAction a = SE Model a

intoAction : Draw a -> GameAction a
intoAction da = SE (\model -> 
        let
            (c, nse) = run da model.seData
        in
            (c, { model | seData = nse }))

type alias ModelUpdate a = a -> Model -> Model

fromUpdate : ModelUpdate a -> (a -> GameAction ())
fromUpdate famm a = SE (pair () << famm a)

bindUpdate : GameAction a -> ModelUpdate a -> GameAction ()
bindUpdate ga = bind ga << fromUpdate

draw : GameAction Card
draw = fmap (\c -> Debug.log ("draw: " ++ Debug.toString c) c) <| intoAction Draw.card

init : Flags -> (Model, Cmd Msg)
init flags = 
    let
        (body, seData) = run (gameBody flags.playerCount) (cards, flags.seed)
        pid = Maybe.withDefault Vector6.Index0 (Vector6.intToIndex flags.rawPlayerId)
    in
        withNone { body = body, seData = seData, pid = pid, selected = Nothing }
