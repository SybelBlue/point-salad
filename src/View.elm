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
import Either exposing (..)

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
    spandext = span [] << singleton << text
    singleText = singleton << spandext
    toString = String.toLower << Veggie.toString
    fromSeq : List (Either String (Html Msg)) -> List (Html Msg)
    fromSeq = singleton << div [ ] << List.map (either spandext identity)
    simpleVeggieImg v = getVeggieImg v False Nothing
  in
    div [ class "objective" ] <|
      case obj of
        Combo vs p -> 
          fromSeq <| List.map (Right << simpleVeggieImg) vs ++ [ Left <| " = " ++ fromInt p ]
        Stacked v n p -> 
          fromSeq <| List.map (Right << simpleVeggieImg) (List.repeat n v) ++ [ Left <| " = " ++ fromInt p ]
        Items vdict -> 
          List.map (div [] << singleText << \(v, p) -> fromInt p ++ " / " ++ toString v) (Veggie.entries vdict)
        Most v p -> 
          fromSeq [Left "most", Right <| simpleVeggieImg v, Left <| "= " ++ fromInt p ]
        Fewest v p -> 
          fromSeq [ Left "fewest", Right <| simpleVeggieImg v, Left <| "= " ++ fromInt p ]
        MostTotal p -> 
          singleText <| "most veggies = " ++ fromInt p
        FewestTotal p -> 
          singleText <| "fewest veggies = " ++ fromInt p
        PerTypeWith n p -> 
          singleText <| fromInt p ++ " / veggie with " ++ fromInt n ++ "+"
        PerMissing p -> 
          singleText <| fromInt p ++ " / missing veggie"
        EvenOdd v e o -> 
          fromSeq <| [ Right <| simpleVeggieImg v, Left <| ": even = " ++ fromInt e ++ "; odd = " ++ fromInt o ]

card : Index -> Card -> Html Msg
card i c =
  div 
    [ class "objective-box" 
    , onClick <| Message.selectObjective i c
    ] 
    [ getVeggieImg (Card.veggie c) False Nothing
    , br [] []
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