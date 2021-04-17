module Main exposing (..)

import Browser
import Html exposing (Html)
import Draw exposing (Draw)
import Game exposing (Game)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


updateGame : Draw a -> Game -> (a, Game)
updateGame da game = 
  let
      ((a, cards), seed) = Draw.runUsing da game.deck game.seed
  in (a, { game | seed = seed, deck = cards })


-- MODEL

type alias Model = ()

init : Model
init = Debug.todo "write meeee"


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