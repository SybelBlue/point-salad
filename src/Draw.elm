module Draw exposing (..)

import Card exposing ( Card )
import Game exposing (..)
import SideEffect exposing (..)
import Veggie exposing ( Veggie )

import Utils exposing ( maybe , uncons )

import Tuple exposing ( mapFirst )
import Vector3 exposing ( from3 )
import Vector6 exposing ( Index (..) )

import Basics.Extra exposing ( flip )

type alias Seed = Int

{-| A generator of random positive integers -}
rand : SE Seed Int
rand = SE (\s ->
    let ns = modBy 0x7FFFFFFF (s * 16807) 
    in (ns, ns) )

type alias Draw a = SE (List Card, Seed) a

card : Draw (Maybe Card)
card = SE (\(cards, seed) -> 
    let
        valid = modBy (List.length cards) << abs
        (n, ns) = mapFirst valid <| run rand seed
        hd = List.take n cards
        (c, tl) = maybe (Nothing, []) (Tuple.mapFirst Just) <| uncons <| List.drop n cards
    in (c, (hd ++ tl, ns)))
    
veggie : Draw (Maybe Veggie)
veggie = fmap (Maybe.map .veggie) card

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