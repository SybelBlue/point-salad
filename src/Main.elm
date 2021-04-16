module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Random exposing (rand)
import Random exposing (run)
import List exposing (repeat)
import List exposing (intersperse)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = 
    { nums: List Int
    , seed: Seed
    }

init : Model
init =
  { nums = repeat 10 0, seed = 12345 }


incrNth : List Int -> Int -> List Int
incrNth ls n = 
    case (ls, n) of
        ([], _) -> []
        (h::t, 0) -> h + 1 :: t
        (h::t, _) -> h :: incrNth t (n - 1)


-- UPDATE

type alias Msg = ()

update : Msg -> Model -> Model
update () model = 
    let randTen = Random.map (modBy 10) rand
        (a, s) = run randTen model.seed
    in { model | seed = s, nums = incrNth model.nums a }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick () ] [ text "+" ]
    , div [ ] [ text <| String.join ", " <| List.map String.fromInt model.nums ]
    ]