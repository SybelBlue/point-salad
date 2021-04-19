module Update exposing (update)

import Model exposing (Model, ModelUpdate, GameAction, bindUpdate, draw)
import Message exposing (Msg(..), Selection)
import Utils exposing (withNone)
import Either exposing (isLeft)
import Card exposing (Card)
import Game exposing (swapCard, givePlayerPicked)
import Basics.Extra exposing (uncurry)
import SideEffect exposing (run)

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
            { model | selected = Nothing }

        Accept ->
            let 
                ((), afterCurr) = run (runSelection s) model
                ((), afterCards) = 
                    case model.selected of
                       Nothing -> ((), afterCurr)
                       Just prevSelection -> run (runSelection prevSelection) model
            in
                { afterCards | selected = Nothing }
        Save r ->
            { model | selected = Just r }
    
