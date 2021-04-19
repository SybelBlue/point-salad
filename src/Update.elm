module Update exposing (update)

import Model exposing (Model, ModelUpdate, GameAction, bindUpdate, draw, clearSelected, simply)
import Message exposing (Msg(..), Selection)
import Utils exposing (withNone)
import Either exposing (isLeft)
import Card exposing (Card)
import Game exposing (swapCard, givePlayerPicked)
import Basics.Extra exposing (uncurry)
import SideEffect exposing (SE(..), do, ifJust)

replacePickedCard : Selection -> ModelUpdate Card
replacePickedCard selection replacement model = 
    let
        giveCurrent = uncurry (givePlayerPicked model.pid)
        newBody = giveCurrent <| swapCard selection replacement model.body
    in
        { model | body = newBody }

runSelection : Selection -> GameAction ()
runSelection = bindUpdate draw << replacePickedCard


type SelectionOutcome
    = Accept
    | Save Selection
    | Cancel

validSelection : Selection -> Model -> SelectionOutcome
validSelection s model =
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
            do  [ ifJust model.selected runSelection
                , runSelection s
                , simply clearSelected
                ]
                model
        Save r ->
            { model | selected = Just r }
    
