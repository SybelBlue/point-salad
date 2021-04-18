module View exposing (view)

import Model exposing (Model)
import Html exposing (Html)
import Message exposing (Msg)
import Html exposing (div, p, text)
import Game exposing (Board)
import Vector3
import Veggie exposing (Veggie)
import Html.Attributes exposing (class)
import Game exposing (Aisle)
import List exposing (singleton)

mapToList : (a -> b) -> Vector3.Vector3 a -> List b
mapToList f = List.map f << Vector3.toList

getVeggieImgPath : Veggie -> String
getVeggieImgPath v = "res/" ++ 
  case v of
    Veggie.Cabbage -> "cabbage.jpeg"
    Veggie.Tomato -> "tomato.jpeg"
    Veggie.Carrot -> "carrot.jpeg"
    Veggie.Pepper -> "pepper.jpeg"
    Veggie.Lettuce -> "lettuce.jpeg"
    Veggie.Onion -> "onion.jpeg"

aisle : Aisle -> Html Msg
aisle (c, (v0, v1)) = 
  div [ class "column" ] <| List.map (p [] << singleton << text) [Debug.toString c, Debug.toString v0, Debug.toString v1]

board : Board -> Html Msg
board =
  div [ class "row" ] << mapToList aisle

view : Model -> Html Msg
view model =
  div []
    [ board model.body.board
    ]