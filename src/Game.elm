module Game exposing (..)

import Card exposing (Card)
import Veggie exposing (Veggie)

type alias Player = 
    { veggies : List Veggie
    , objectiveCards : List Card
    , id : Int
    }


type alias Game = 
    { players : List Player
    , playing : Int
    , deck : List Card
    }


