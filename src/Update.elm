module Update exposing (..)

import Model exposing (Model)
import Message exposing (Msg)
import Utils exposing (withNone)

update : Msg -> Model -> (Model, Cmd Msg)
update () = withNone << identity
