module Draw exposing ( Draw , Seed , card , gameBody , rand )

import Card exposing ( Card )
import Game exposing (..)
import SideEffect exposing (..)
import Veggie exposing ( Veggie )

import Utils exposing ( maybe , uncons )

import Tuple exposing ( mapFirst )
import Vector3 exposing ( from3 )
import Vector6 exposing ( Index (..) )

import Basics.Extra exposing ( flip , safeModBy )

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
            valid = safeModBy (List.length cards) << abs
            (mn, ns) = mapFirst valid <| run rand seed
            nextCard n =
                let 
                    hd = List.take n cards
                    (mc, tl) = maybe (Nothing, []) (Tuple.mapFirst Just) <| uncons <| List.drop n cards
                in 
                    (mc, hd ++ tl)
            (mcard, ncards) = maybe (Nothing, cards) nextCard mn
        in 
            (mcard, (ncards, ns))
    )
    
    
veggie : Draw (Maybe Veggie)
veggie = fmap (Maybe.map .veggie) card

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