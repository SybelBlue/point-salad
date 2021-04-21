module Test exposing (..)

import Game exposing (..)
import Vector6 exposing (..)
import Card exposing (Objective(..))
import Veggie exposing (Veggie(..))

testVegs = from6 
    [ Tomato, Tomato, Tomato ]
    [ Tomato, Carrot, Tomato ]
    [ Tomato, Tomato, Tomato, Cabbage ]
    [ Carrot ]
    []
    []

unwrapJust : Maybe a -> a
unwrapJust x =
    case x of
       Just a ->
            a

       Nothing ->
            Debug.todo "you get NOTHING"

testScoreObjective o = List.map (\p -> scoreObjective testVegs p o) (Vector6.toList Vector6.indices)
