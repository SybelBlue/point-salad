module Draw exposing (..)

import Tuple exposing (mapFirst)

import SideEffect exposing (..)
import Game exposing (..)
import Vector6 exposing (Index(..))
import Card exposing (Card)
import Veggie exposing (Veggie)
import Utils exposing (unconsOrDie)

import Vector3 exposing (from3)
import Basics.Extra exposing (flip)

type alias Seed = Int

{-| A generator of random positive integers -}
rand : SE Seed Int
rand = SE (\s ->
    let ns = modBy 0x7FFFFFFF (s * 16807) 
    in (ns, ns) )

type alias Draw a = SE (List Card, Seed) a

card : Draw Card
card = SE (\(cards, seed) -> 
    let
        valid = modBy (List.length cards) << abs
        (n, ns) = mapFirst valid <| run rand seed
        hd = List.take n cards
        (c, tl) = unconsOrDie "bad card gen" <| List.drop n cards
    in (c, (hd ++ tl, ns)))
    

veggie : Draw Veggie
veggie = fmap .veggie card

aisle : Draw Aisle
aisle = liftA3 Game.aisle card veggie veggie

board : Draw Board
board = liftA3 from3 aisle aisle aisle

gameBody : Int -> Draw GameBody
gameBody playerCount = flip fmap board <| 
    \b ->
        { players = makePlayers playerCount
        , board = b
        , playing = Index0 
        }