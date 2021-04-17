module Main exposing (..)

import Browser
import Html exposing (Html)
import Model exposing (..)



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


-- VIEW

view : Model -> Html Msg
view _ = Debug.todo "write meeee"
  -- div []
  --   [ button [ onClick () ] [ text "+" ]
  --   , div [ ] [ text <| String.join ", " <| List.map String.fromInt model.nums ]
  --   ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none