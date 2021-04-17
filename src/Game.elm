module Game exposing (..)

import Card exposing (Card)
import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)

type alias Player = 
    { veggies : List Veggie
    , objectiveCards : List Card
    , id : Int
    }

newPlayer : Int -> Player
newPlayer id = 
    { veggies = [], objectiveCards = [], id = id }


type alias Aisle = Vector3 Card

type alias Board = Vector3 Aisle

type alias Game = 
    { players : List Player
    , playing : Int
    , deck : List Card
    , board : Board
    }
