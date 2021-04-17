module View exposing (..)

import Model exposing (Model)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (div)
import Html exposing (text)


view : Model -> Html Msg
view model =
  div []
    [ div [ ] [ text "Board:" ]
    , div [ ] [ text <| Debug.toString model.body.board ]
    ]