module Update exposing (..)

import Model exposing (Model)
import Message exposing (Msg(..), Selection)
import Utils exposing (withNone)
import Either exposing (isLeft)
import Card exposing (Card)
import Model exposing (ModelUpdate)
import Game exposing (swapCard)
import Game exposing (givePlayerPicked)
import Basics.Extra exposing (uncurry)

replacePickedCard : Selection -> ModelUpdate Card
replacePickedCard selection replacement model = 
    let
        giveCurrent = uncurry (givePlayerPicked model.pid)
        newBody = giveCurrent <| swapCard selection replacement model.body
    in
        { model | body = newBody }

update : Msg -> Model -> (Model, Cmd Msg)
update (Selected s) model = withNone <| 
    if isLeft s.item
        then 
            (if model.selected == Nothing
                then Debug.log ("accept obj: " ++ Debug.toString s) identity
                else identity)
                { model | selected = Nothing }
        else case model.selected of
            Nothing -> { model | selected = Just s }
            Just ms -> 
                (if ms == s 
                    then identity
                    else Debug.log ("accept veg:" ++ Debug.toString (ms, s)) identity)
                    { model | selected = Nothing }
    
