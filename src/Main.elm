module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = ()

init : Model
init = Debug.todo "write meeee"


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> Model
update msg model = Debug.todo "write meeee"


-- VIEW

view : Model -> Html Msg
view model = Debug.todo "write meeee"
  -- div []
  --   [ button [ onClick () ] [ text "+" ]
  --   , div [ ] [ text <| String.join ", " <| List.map String.fromInt model.nums ]
  --   ]