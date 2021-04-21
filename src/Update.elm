module Update exposing ( update )

import Card exposing ( Card )
import Game exposing ( advancePlayer , givePlayerPicked , swapCard )
import Message exposing ( Msg ( .. ) , Selection )
import Model exposing ( GameAction , Model , ModelUpdate , basically , bindUpdate , clearSelected , draw , simply )
import SideEffect exposing ( SE ( .. ) , do , ifJust )

import Either exposing ( isLeft )
import Utils exposing ( withNone )

import Basics.Extra exposing ( uncurry )

replacePickedCard : Selection -> ModelUpdate Card
replacePickedCard selection replacement model = 
    let
        giveCurrent = uncurry (givePlayerPicked model.body.playing)
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
                , basically advancePlayer
                ]
                model

        Save r ->
            { model | selected = Just r }
    
