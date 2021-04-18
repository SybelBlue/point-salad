module Update exposing (..)

import Model exposing (Model)
import Message exposing (Msg)
import Utils exposing (withNone)
import Message exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update (Selected s) model = withNone <| (Debug.log (Debug.toString s) identity) model
    
