module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Vector3 exposing (Index, indexedMap)

import Model exposing (Model)
import Message exposing (Msg)
import Game exposing (Aisle, Board)
import Veggie exposing (Veggie)
import Card exposing (Card(..))

getVeggieImgPath : Veggie -> String
getVeggieImgPath v = "res/" ++ String.toLower (Veggie.toString v) ++ ".jpeg"

getVeggieImg : Veggie -> Bool -> Msg -> Html Msg
getVeggieImg v big m = 
  img 
    [ class <| "veggie " ++ if big then "big" else "small"
    , src <| getVeggieImgPath v
    , alt <| Veggie.toString v
    , onClick m
    ] 
    []

card : Index -> Card -> Html Msg
card i (Card id veg obj) =
  let
      m = Message.selectObjective i (Card id veg obj)
  in
    div 
      [ class "objective" 
      , onClick m
      ] 
      [ getVeggieImg veg False m
      , text (Debug.toString obj)
      ]

aisle : Index -> Aisle -> Html Msg
aisle i (c, (v0, v1)) = 
  div 
    [ class "column" ] 
    [ card i c
    , hr [] []
    , getVeggieImg v0 True <| Message.Selected { item = Ok { veggie = v0, first = True }, aisle = i }
    , getVeggieImg v1 True <| Message.Selected { item = Ok { veggie = v1, first = False }, aisle = i }
    ]

board : Board -> Html Msg
board =
  div [ class "row" ] << Vector3.toList << indexedMap aisle

view : Model -> Html Msg
view model =
  div []
    [ board model.body.board
    ]