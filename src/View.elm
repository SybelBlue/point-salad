module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import String exposing (fromInt)
import List exposing (singleton)
import Vector3 exposing (Index, indexedMap)

import Model exposing (Model)
import Message exposing (Msg)
import Game exposing (Aisle, Board)
import Veggie exposing (Veggie)
import Card exposing (Card(..), Objective(..))

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

objective : Objective -> Msg -> Html Msg
objective obj msg =
  div [ class "objective", onClick msg ] <|
    case obj of
      Combo vs ps -> 
        [ div [ ] <| List.map (\v -> getVeggieImg v False msg) vs ++ [ text <| " = " ++ fromInt ps] ]
      Stacked v n p -> singleton <| text <| fromInt p ++ " / " ++ fromInt n ++ " " ++ Veggie.toString v ++ "s"
      Items vdict -> List.map (div [] << singleton << text << \(v, p) -> fromInt p ++ " / " ++ Veggie.toString v) <| Veggie.entries vdict
      Most v p -> singleton <| text <| "most " ++ Veggie.toString v ++ " = " ++ fromInt p
      Fewest v p -> singleton <| text <| "fewest " ++ Veggie.toString v ++ " = " ++ fromInt p
      MostTotal p -> singleton <| text <| "most veggies = " ++ fromInt p
      FewestTotal p -> singleton <| text <| "fewest veggies = " ++ fromInt p
      PerTypeWith n p -> singleton <| text <| fromInt p ++ " / veggie with " ++ fromInt n ++ "+"
      PerMissing p -> singleton <| text <| fromInt p ++ " / missing veggie"
      EvenOdd v e o -> singleton <| text <| "even " ++ Veggie.toString v ++ "s = " ++ fromInt e ++ ", odd" ++ Veggie.toString v ++ "s = " ++ fromInt o

card : Index -> Card -> Html Msg
card i (Card id veg obj) =
  let
      m = Message.selectObjective i (Card id veg obj)
  in
    div 
      [ class "objective-box" 
      , onClick m
      ] 
      [ getVeggieImg veg False m
      , objective obj m
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