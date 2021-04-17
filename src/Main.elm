module Main exposing (..)

import Browser
import Html exposing (Html)
import Model exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }




-- UPDATE

type alias Msg = ()

update : Msg -> Model -> Model
update _ _ = Debug.todo "write meeee"


-- VIEW

view : Model -> Html Msg
view _ = Debug.todo "write meeee"
  -- div []
  --   [ button [ onClick () ] [ text "+" ]
  --   , div [ ] [ text <| String.join ", " <| List.map String.fromInt model.nums ]
  --   ]