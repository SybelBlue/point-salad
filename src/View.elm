module View exposing (..)

import Model exposing (Model)
import Html exposing (Html)
import Message exposing (Msg)



-- VIEW

view : Model -> Html Msg
view _ = Debug.todo "write meeee"
  -- div []
  --   [ button [ onClick () ] [ text "+" ]
  --   , div [ ] [ text <| String.join ", " <| List.map String.fromInt model.nums ]
  --   ]