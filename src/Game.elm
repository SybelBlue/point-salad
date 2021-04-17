module Game exposing (..)

import Card exposing (Card)
import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)
import Vector6 exposing (Vector6, Index(..))

type alias Player = 
    { veggies : List Veggie
    , objectiveCards : List Card
    , id : Int
    }

newPlayer : Int -> Player
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

type alias Game = 
    { players : Vector6 (Maybe Player)
    , playing : Index
    , deck : List Card
    , board : Board
    }
