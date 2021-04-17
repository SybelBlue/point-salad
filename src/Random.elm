module Random exposing (..)

import SideEffect exposing (..)

type alias Seed = Int

{-| A monadic generic random object generator -}
type alias Gen a = SE Seed a

{-| A generator of random positive integers -}
rand : Gen Int
rand = SE (\s ->
    let ns = modBy 0x7FFFFFFF (s * 16807) 
    in (ns, ns) )
