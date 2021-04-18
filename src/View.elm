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
import Utils exposing (maybeAsList)

getVeggieImgPath : Veggie -> String
getVeggieImgPath v = "res/" ++ String.toLower (Veggie.toString v) ++ ".jpeg"

getVeggieImg : Veggie -> Bool -> Maybe Msg -> Html Msg
getVeggieImg v big m = 
  img 
    ([ class <| "veggie " ++ if big then "big" else "small"
     , src <| getVeggieImgPath v
     , alt <| Veggie.toString v
     ] ++ maybeAsList (Maybe.map onClick m))
    []

objective : Objective -> Html Msg
objective obj =
  let  
    singleText = singleton << span [] << singleton << text
    toString = String.toLower << Veggie.toString
  in
    div [ class "objective" ] <|
      case obj of
        Combo vs ps -> 
          [ div [ ] <| List.map (\v -> getVeggieImg v False Nothing) vs ++ (singleText <| " = " ++ fromInt ps) ]
        Stacked v n p -> 
          singleText <| fromInt p ++ " / " ++ fromInt n ++ " " ++ toString v ++ "s"
        Items vdict -> 
          List.map (div [] << singleText << \(v, p) -> fromInt p ++ " / " ++ toString v) (Veggie.entries vdict)
        Most v p -> 
          singleText <| "most " ++ toString v ++ " = " ++ fromInt p
        Fewest v p -> 
          singleText <| "fewest " ++ toString v ++ " = " ++ fromInt p
        MostTotal p -> 
          singleText <| "most veggies = " ++ fromInt p
        FewestTotal p -> 
          singleText <| "fewest veggies = " ++ fromInt p
        PerTypeWith n p -> 
          singleText <| fromInt p ++ " / veggie with " ++ fromInt n ++ "+"
        PerMissing p -> 
          singleText <| fromInt p ++ " / missing veggie"
        EvenOdd v e o -> 
          singleText <| "even " ++ toString v ++ "s = " ++ fromInt e ++ ", odd" ++ toString v ++ "s = " ++ fromInt o

card : Index -> Card -> Html Msg
card i c =
  div 
    [ class "objective-box" 
    , onClick <| Message.selectObjective i c
    ] 
    [ getVeggieImg (Card.veggie c) False Nothing
    , objective <| Card.objective c 
    ]

aisle : Index -> Aisle -> Html Msg
aisle i (c, (v0, v1)) = 
  div 
    [ class "column" ] 
    [ card i c
    , hr [] []
    , getVeggieImg v0 True <| Just <| Message.Selected { item = Ok { veggie = v0, first = True }, aisle = i }
    , getVeggieImg v1 True <| Just <| Message.Selected { item = Ok { veggie = v1, first = False }, aisle = i }
    ]

board : Board -> Html Msg
board =
  div [ class "row" ] << Vector3.toList << indexedMap aisle

view : Model -> Html Msg
view model =
  div []
    [ board model.body.board
    ]