module Main exposing (..)

import Browser
import Model exposing (..)
import View exposing (view)



-- MAIN


main : Program Flags Model Msg
main = Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }




-- UPDATE

type alias Msg = ()

update : Msg -> Model -> (Model, Cmd Msg)
update _ _ = Debug.todo "write meeee"



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none