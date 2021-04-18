module View exposing (view)

import Html exposing (Html, div, p, text, img)
import Html.Attributes exposing (..)

import Vector3

import Model exposing (Model)
import Message exposing (Msg)
import Game exposing (Board)
import Veggie exposing (Veggie)
import Game exposing (Aisle)

mapToList : (a -> b) -> Vector3.Vector3 a -> List b
mapToList f = List.map f << Vector3.toList

getVeggieImgPath : Veggie -> String
getVeggieImgPath v = "res/" ++ String.toLower (Veggie.toString v) ++ ".jpeg"

getVeggieImg : Veggie -> Bool -> Html Msg
getVeggieImg v big = 
  img 
    [ class <| "veggie " ++ if big then "big" else "small"
    , src <| getVeggieImgPath v
    , alt <| Veggie.toString v
    ] 
    []

aisle : Aisle -> Html Msg
aisle (c, (v0, v1)) = 
  div 
    [ class "column" ] 
    [ p [] [ text <| Debug.toString c ]
    , getVeggieImg v0 True
    , getVeggieImg v1 True
    ]

board : Board -> Html Msg
board =
  div [ class "row" ] << mapToList aisle

view : Model -> Html Msg
view model =
  div []
    [ board model.body.board
    ]