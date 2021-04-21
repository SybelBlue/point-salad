module Main exposing ( .. )

import Browser
import Message exposing ( Msg )
import Model exposing ( .. )
import Update exposing ( update )
import View exposing ( view )

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