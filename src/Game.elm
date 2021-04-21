module Game exposing (..)

import Veggie exposing (Veggie)
import Vector3 exposing (Vector3)
import Vector6 exposing (Vector6, Index(..), nextIndex)
import Message exposing (Selection)
import Either exposing (Either(..), either)
import Card exposing (Card, Objective(..), isGlobalObjective)
import Basics.Extra exposing (flip, safeIntegerDivide)
import Utils exposing (maybe, count)


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
makePlayers n = Vector6.initializeFromIndex (\i -> if Vector6.indexToInt i < n then Just (newPlayer i) else Nothing)

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
                        cv = c.veggie
                        (nv0, nv1) = if vegFirst.first then (cv, v1) else (v0, cv)
                    in
                        (Right vegFirst.veggie, (oldObj, nv0, nv1))
        newBoard = Vector3.set s.aisle newAisle body.board
    in
        (oldItem, { body | board = newBoard })

givePlayerPicked : PlayerId -> Either Card Veggie -> GameBody -> GameBody
givePlayerPicked pid ecv body =
    case Vector6.get pid body.players of
        Nothing -> body
        Just oldPlayer ->
            let
                nplayer = either 
                        (\c p -> { p | objectiveCards = c :: p.objectiveCards })
                        (\v p -> { p | veggies = v :: p.veggies })
                        ecv
                        oldPlayer
            in { body | players = Vector6.set pid (Just nplayer) body.players }

scoreObjective : Vector6 (List Veggie) -> PlayerId -> Objective -> Int
scoreObjective veggies pid obj =
    let
        pVeggies = Vector6.get pid veggies
        numOfVeggie v = List.length << List.filter (\x -> v == x)
        
        isBest : (List Int -> Maybe Int) -> (List Veggie -> Int) -> Int -> Int
        isBest folder scorer p = 
            let
                best = Maybe.withDefault 0 << folder << Vector6.toList << Vector6.map scorer
            in
                if scorer pVeggies == best veggies
                    then p
                    else 0
    in 
        case obj of
            Most v p -> 
                isBest List.maximum (numOfVeggie v) p

            Fewest v p ->
                isBest List.minimum (numOfVeggie v) p

            MostTotal p ->
                isBest List.maximum List.length p

            FewestTotal p ->
                isBest List.minimum List.length p

            Combo vs p ->
                maybe 0 ((*) p) <| List.minimum <| List.map (flip numOfVeggie pVeggies) vs

            Stacked v n p ->
                maybe 0 ((*) p) <| flip safeIntegerDivide n <| numOfVeggie v pVeggies

            Items vd ->
                List.sum <| List.map (\(v, p) -> p * numOfVeggie v pVeggies) <| Veggie.entries vd

            PerTypeWith n p ->
                p * (List.length <| List.filter ((\(_, x) -> x >= n)) <| count <| pVeggies)

            PerMissing p ->
                p * (6 - (List.length <| count <| pVeggies))

            EvenOdd v e o ->
                if modBy 2 (numOfVeggie v pVeggies) == 0 then e else o

scores : GameBody -> Vector6 Int
scores gbody =
    let
        globalObjs = 
            List.filter isGlobalObjective <|
             List.map .objective <|
              List.concatMap (maybe [] (.objectiveCards)) <| 
               Vector6.toList gbody.players
        veggies = Vector6.map (maybe [] (.veggies)) gbody.players
        scoreP player = 
            let
                personalObjs = 
                    List.filter (not << isGlobalObjective) <| 
                      List.map .objective player.objectiveCards
            in
                List.sum <| 
                 List.map (scoreObjective veggies player.id) <| 
                  globalObjs ++ personalObjs
    in
        Vector6.map (maybe 0 scoreP) gbody.players
