module Model exposing ( .. )

import Card exposing ( Card )
import Cards exposing ( cards )
import Draw exposing ( Draw , Seed , gameBody )
import Game exposing ( GameBody , PlayerId )
import Message exposing ( Msg , Selection )
import SideEffect exposing ( .. )

import Utils exposing ( withNone )

import Tuple exposing ( pair )
import Vector6

{-| (# of Players, this player's id, start seed) -}
type alias Flags = 
    { playerCount : Int
    , rawPlayerId : Int
    , seed : Seed
    }

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

simply : (Model -> Model) -> GameAction ()
simply mapping = SE (pair () << mapping)

basically : (GameBody -> GameBody) -> GameAction ()
basically f = simply (\model -> { model | body = f model.body })

fromUpdate : ModelUpdate a -> (a -> GameAction ())
fromUpdate aToMapping a = SE (pair () << aToMapping a)

bindUpdate : GameAction a -> ModelUpdate a -> GameAction ()
bindUpdate ga = bind ga << fromUpdate

draw : GameAction Card
draw = intoAction Draw.card

clearSelected : Model -> Model
clearSelected model = 
    { model | selected = Nothing }

init : Flags -> (Model, Cmd Msg)
init flags = 
    let
        (body, seData) = run (gameBody flags.playerCount) (cards, flags.seed)
        pid = Maybe.withDefault Vector6.Index0 (Vector6.intToIndex flags.rawPlayerId)
    in
        withNone { body = body, seData = seData, pid = pid, selected = Nothing }
