module Random exposing (..)

import Tuple exposing (mapFirst, pair)

import Basics.Extra exposing (uncurry)

type alias Seed = Int

{-| A monadic generic random object generator -}
type Gen a = Gen (Seed -> (a, Seed))

{-| Runs a generator on a seed -}
run : Gen a -> Seed -> (a, Seed)
run (Gen f) = f

{-| Applies a function across a Gen Monad -}
map : (a -> b) -> Gen a -> Gen b
map fab ga = Gen (mapFirst fab << run ga)

ap : Gen (a -> b) -> Gen a -> Gen b
ap gfab ga = Gen (\s ->
    let (fab, ns)  = run gfab s
    in mapFirst fab <| run ga ns) 

bind : Gen a -> (a -> Gen b) -> Gen b
bind ga fagb = Gen (uncurry run << mapFirst fagb << run ga)

return : a -> Gen a
return = Gen << pair

randf s = 
    let ns = modBy 0x7FFFFFFF (s * 16807) 
    in (ns, ns)

rand : Gen Int
rand = Gen randf