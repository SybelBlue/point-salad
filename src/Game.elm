module Game exposing (..)

import Tuple exposing (mapFirst, pair)

import Card exposing (Card)
import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)
import Random exposing (Gen(..), rand, fmap, bind)
import Utils exposing (unconsOrDie)

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


randomCard : List Card -> Gen (Card, List Card)
randomCard cards = 
    let
        valid = modBy (List.length cards) << abs
        getNth n = 
            let
                hd = List.take n cards
                (c, tl) = unconsOrDie "bad card gen" <| List.drop n cards
            in (c, hd ++ tl)
    in fmap (getNth << valid) rand
    
randomCardPair : List Card -> Gen ((Card, Card), List Card)
randomCardPair cards =
    let
        secondGen : (Card, List Card) -> Gen ((Card, Card), List Card)
        secondGen (c, cs) = fmap (mapFirst <| pair c) (randomCard cs)
    in bind (randomCard cards) secondGen