module Update exposing (update)

import Model exposing (Model, ModelUpdate, GameAction, bindUpdate, draw, clearSelected, simply)
import Message exposing (Msg(..), Selection)
import Utils exposing (withNone, maybe)
import Either exposing (isLeft)
import Card exposing (Card)
import Game exposing (swapCard, givePlayerPicked)
import Basics.Extra exposing (uncurry)
import SideEffect exposing (run)
import Tuple exposing (pair)
import SideEffect exposing (SE(..), do)

replacePickedCard : Selection -> ModelUpdate Card
replacePickedCard selection replacement model = 
    let
        giveCurrent = uncurry (givePlayerPicked model.pid)
        newBody = giveCurrent <| swapCard selection replacement model.body
    in
        { model | body = newBody }

runSelection : Selection -> GameAction ()
runSelection s = Debug.log ("running: " ++ Debug.toString s) << bindUpdate draw << replacePickedCard <| s


type SelectionOutcome
    = Accept
    | Save Selection
    | Cancel

validSelection : Selection -> Model -> SelectionOutcome
validSelection s model = Debug.log ("clicked: " ++ Debug.toString s) <|
    if isLeft s.item
        then
            if model.selected == Nothing 
                then Accept
                else Cancel
        else
            case model.selected of
                Nothing -> 
                    Save s
                Just ms -> 
                    if ms == s
                        then Cancel
                        else Accept

update : Msg -> Model -> (Model, Cmd Msg)
update (Selected s) model = withNone <| 
    case validSelection s model of
        Cancel -> 
            clearSelected model

        Accept ->
            let 
                prevCard = maybe (pair ()) (run << runSelection) model.selected
            in 
                do [ SE prevCard
                   , runSelection s
                   , simply clearSelected
                   ]
                   model
        Save r ->
            { model | selected = Just r }
    
