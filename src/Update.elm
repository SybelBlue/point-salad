module Update exposing (..)

import Model exposing (Model)
import Message exposing (Msg)
import Utils exposing (withNone)
import Message exposing (Msg(..))
import Either exposing (isLeft)

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
    
