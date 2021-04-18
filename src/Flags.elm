module Flags exposing (..)
import Draw exposing (Seed)

{-| (# of Players, this player's id, start seed) -}
type alias Flags = 
    { playerCount : Int
    , rawPlayerId : Int
    , seed : Seed
    }