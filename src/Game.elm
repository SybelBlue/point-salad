module Game exposing (..)

import Card exposing (Card)
import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)
import Vector6 exposing (Vector6, Index(..), nextIndex)


type alias PlayerId = Int

type alias Player = 
    { veggies : List Veggie
    , objectiveCards : List Card
    , id : PlayerId
    }

newPlayer : PlayerId -> Player
newPlayer id = 
    { veggies = [], objectiveCards = [], id = id }


type alias Aisle = (Card, (Veggie, Veggie))

aisle : Card -> Veggie -> Veggie -> Aisle
aisle c v0 v1 = (c, (v0, v1))

type Move = Move
    { pick : Result (Veggie, Veggie) Card
    , flipped : Maybe Card
    }

type alias Board = Vector3 Aisle

type alias GameBody = 
    { players : Vector6 (Maybe Player)
    , playing : Index
    , board : Board
    }

makePlayers : Int -> Vector6 (Maybe Player)
makePlayers n = Vector6.initializeFromInt (\i -> if i + 1 < n then Just (newPlayer i) else Nothing)

advancePlayer : GameBody -> GameBody
advancePlayer game = 
    let
        next = { game | playing = Maybe.withDefault Index0 <| nextIndex game.playing }
    in
        if Vector6.get next.playing next.players == Nothing 
            then advancePlayer next
            else next
