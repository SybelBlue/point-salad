module Main exposing (..)

import Browser
import Model exposing (..)
import View exposing (view)
import Update exposing (update)
import Message exposing (Msg)



-- MAIN


main : Program Flags Model Msg
main = Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none