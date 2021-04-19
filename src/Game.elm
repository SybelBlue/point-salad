module Game exposing (..)

import Card exposing (Card)
import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)
import Vector6 exposing (Vector6, Index(..), nextIndex)
import Message exposing (Selection)
import Either exposing (Either(..), either)


type alias PlayerId = Index

type alias Player = 
    { veggies : List Veggie
    , objectiveCards : List Card
    , id : PlayerId
    }

newPlayer : PlayerId -> Player
newPlayer id = 
    { veggies = [], objectiveCards = [], id = id }


type alias Aisle = (Card, Veggie, Veggie)

aisle : Card -> Veggie -> Veggie -> Aisle
aisle c v0 v1 = (c, v0, v1)

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
makePlayers n = Debug.log "players" <| Vector6.initializeFromIndex (\i -> if Vector6.indexToInt i < n then Just (newPlayer i) else Nothing)

advancePlayer : GameBody -> GameBody
advancePlayer game = 
    let
        next = { game | playing = Maybe.withDefault Index0 <| nextIndex game.playing }
    in
        if Vector6.get next.playing next.players == Nothing 
            then advancePlayer next
            else next

swapCard : Selection -> Card -> GameBody -> (Either Card Veggie, GameBody)
swapCard s c body =
    let
        (oldObj, v0, v1) = Vector3.get s.aisle body.board
        (oldItem, newAisle) = 
            case s.item of
                Left oldc -> (Left oldc, (c, v0, v1))
                Right vegFirst -> 
                    let
                        cv = Card.veggie c
                        (nv0, nv1) = if vegFirst.first then (cv, v1) else (v0, cv)
                    in
                        (Right vegFirst.veggie, (oldObj, nv0, nv1))
        newBoard = Vector3.set s.aisle newAisle body.board
    in
        (oldItem, { body | board = newBoard })

givePlayerPicked : PlayerId -> Either Card Veggie -> GameBody -> GameBody
givePlayerPicked pid ecv body =
    case Vector6.get pid body.players of
        Nothing -> Debug.log ("err: " ++ Debug.toString (pid, ecv, body)) body
        Just oldPlayer ->
            let
                nplayer = either 
                        (\c p -> { p | objectiveCards = c :: p.objectiveCards })
                        (\v p -> { p | veggies = v :: p.veggies })
                        ecv
                        oldPlayer
            in { body | players = Vector6.set pid (Just <| Debug.log "giving" nplayer) body.players }
    