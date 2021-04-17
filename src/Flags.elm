module Flags exposing (..)
import Game exposing (PlayerId)
import Draw exposing (Seed)

{-| (# of Players, this player's id, start seed) -}
type alias Flags = 
    { playerCount : Int
    , playerId : PlayerId
    , seed : Seed
    }