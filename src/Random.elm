module Random exposing (..)

import Tuple exposing (mapFirst, pair)

import Basics.Extra exposing (uncurry)

type alias Seed = Int

{-| A monadic generic random object generator -}
type Gen a = Gen (Seed -> (a, Seed))

{-| Runs a generator on a seed -}
run : Gen a -> Seed -> (a, Seed)
run (Gen f) = f

{-| Applies a function across a Gen Monad.
    see Haskell's Functor fmap or (<$>) -}
map : (a -> b) -> Gen a -> Gen b
map fab ga = Gen (mapFirst fab << run ga)

{-| Applies a function generator across a parameter generator,
    see Haskell's Control.Applicative (<*>) -}
ap : Gen (a -> b) -> Gen a -> Gen b
ap gfab ga = Gen (\s ->
    let (fab, ns)  = run gfab s
    in mapFirst fab <| run ga ns) 

{-| See Haskell's Monad.bind (>>=) -}
bind : Gen a -> (a -> Gen b) -> Gen b
bind ga fagb = Gen (uncurry run << mapFirst fagb << run ga)

{-| Creates a generator from a value -}
return : a -> Gen a
return = Gen << pair

{-| A generator of random positive integers -}
rand : Gen Int
rand = Gen (\s ->
    let ns = modBy 0x7FFFFFFF (s * 16807) 
    in (ns, ns) )

{-| Designed to easily sequence runs, eg

    run rand >> thenRun rand 
 -}
thenRun : Gen b -> (a, Seed) -> ((a, b), Seed)
thenRun gb (a, s) = mapFirst (pair a) <| run gb s

{-| Lifts a function so that it operates on and returns generators, eg

    liftA2 (::) rand (repeated rand 4)
 -}
liftA2 : (a -> b -> c) -> (Gen a -> Gen b -> Gen c)
liftA2 fabc = ap << map fabc

{-| Sequentially generates from a single generator n times -}
repeated : Gen a -> Int -> Gen (List a)
repeated ga n = 
    if n <= 0 then return [] else 
        liftA2 (::) ga <| repeated ga (n - 1)
