module View exposing ( view )

import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Html.Events exposing ( onClick )

import Card exposing ( Objective ( .. ) , Card )
import Game exposing ( Aisle , Board , Player , PlayerId , scores )
import List exposing ( singleton )
import Message exposing ( Msg )
import Model exposing ( Model )
import String exposing ( fromInt )
import Veggie exposing ( Veggie )

import Either exposing ( .. )
import Utils exposing ( count , maybeAsList )

import Vector3 exposing ( Index , indexedMap )
import Vector6 exposing ( Vector6 )

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

spandext : String -> Html msg
spandext = span [] << singleton << text

fromSeq : List (Either String (Html Msg)) -> List (Html Msg)
fromSeq = singleton << div [ ] << List.map (either spandext identity)

objective : Objective -> Html Msg
objective obj =
  let  
    singleText = singleton << spandext
    simpleVeggieImg v = getVeggieImg v False Nothing
  in
    div [ class "objective" ] <|
      case obj of
        Combo vs p -> 
          fromSeq <| (List.intersperse (Left " + ") <| List.map (Right << simpleVeggieImg) vs) ++ [ Left <| " = " ++ fromInt p ]
        Stacked v n p -> 
          fromSeq <| (List.intersperse (Left " + ") <| List.map (Right << simpleVeggieImg) (List.repeat n v)) ++ [ Left <| " = " ++ fromInt p ]
        Items vdict -> 
          fromSeq <| List.concatMap (\(v, p) -> [ Left <| fromInt p ++ " / ", Right <| simpleVeggieImg v ]) (Veggie.entries vdict)
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
          fromSeq [ Right <| simpleVeggieImg v, Left <| ": even = " ++ fromInt e ++ "; odd = " ++ fromInt o ]

card : Index -> Card -> Html Msg
card i c =
  div 
    [ class "objective-box" 
    , onClick <| Message.selectObjective i c
    ] 
    [ getVeggieImg (c.veggie) False Nothing
    , br [] []
    , objective c.objective
    ]

aisle : Index -> Aisle -> Html Msg
aisle i (c, v0, v1) = 
  div 
    [ class "column" ]
    [ card i c
    , hr [] []
    , getVeggieImg v0 True <| Just <| Message.Selected { item = Right { veggie = v0, first = True }, aisle = i }
    , getVeggieImg v1 True <| Just <| Message.Selected { item = Right { veggie = v1, first = False }, aisle = i }
    ]

board : Board -> Html Msg
board =
  div [ class "row" ] << Vector3.toList << indexedMap aisle

player : Vector6 Int -> PlayerId -> Player -> Html Msg
player scores playing p = 
  let 
    vcount = count p.veggies
    sorted = List.sortBy (Veggie.toInt << Tuple.first) vcount
    heading = if playing == p.id then b else div
    vegMap (v, n) = [ Left <| fromInt n ++ "x", Right <| getVeggieImg v False Nothing ]
  in div [] <|
    [ heading [] [ text <| "Player " ++ fromInt (1 + Vector6.indexToInt p.id) ++ "  " ++ fromInt (Vector6.get p.id scores) ++ "pts" ] 
    , div [] <| fromSeq <| List.concatMap vegMap sorted
    , div [] <| List.map (objective << .objective) p.objectiveCards
    , hr [] []
    ]

view : Model -> Html Msg
view model =
  let
      ps = List.filterMap identity <| Vector6.toList model.body.players
      scs = scores model.body
  in div [] <| 
    board model.body.board :: List.map (player scs model.body.playing) ps

