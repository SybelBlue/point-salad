module SideEffect exposing ( .. )

import Utils exposing ( maybe )

import List exposing ( foldl )
import Tuple exposing ( mapFirst , pair , second )

import Basics.Extra exposing ( flip , uncurry )

{-| A monadic generic side effect isolator 

    (result, nextSideEffect) = run func sideEfect
-}
type SE se res = SE (se -> (res, se))

{-| Unwraps a side-effect object -}
run : SE se res -> se -> (res, se)
run (SE f) = f

{-| Composes to side effects, ignoring the 
    result of the first (but chaining side effects),
    see Haskell's (>>)
-}
seq : SE se a -> SE se b -> SE se b
seq sa sb = SE (run sa >> second >> run sb)

{-| Folds left over as with a starting b to create a
    using a folding function to create Se se b
-}
foldSe : (a -> b -> SE se b) -> b -> List a -> SE se b
foldSe fabsb = foldl (flip bind << fabsb) << return

{-| Sequentially composes simple side effect operations -}
do : List (SE se ()) -> se -> se
do sunits =
    case sunits of
       [] -> identity
       sunit :: rest -> do rest << second << run sunit

{-| Primarily for use in a do block, if (Just x) then 
    the function will run, else just return ()
-}
ifJust : Maybe a -> (a -> SE se ()) -> SE se ()
ifJust = flip (maybe <| return ())

{-| Returns a new side effect object -}
return : res -> SE se res
return = SE << pair

{-| Maps the result of a side effect object -}
fmap : (a -> b) -> SE se a -> SE se b
fmap fab sa = SE (mapFirst fab << run sa)

{-| Applies a side-effect function to side-effect parameter and produces
    a side effect result 
-}
ap : SE se (a -> b) -> SE se a -> SE se b
ap sfab sa = SE (\s -> 
        let
            (fab, ns) = run sfab s
        in
            mapFirst fab <| run sa ns)

{-| See Haskell's Monad.bind (>>=) -}
bind : SE se a -> (a -> SE se b) -> SE se b
bind sa fasb = SE (uncurry run << mapFirst fasb << run sa)

{-| Lifts a pure bifunction to operate on side-effect parameters -}
liftA2 : (a -> b -> c) -> SE se a -> SE se b -> SE se c
liftA2 fabc = ap << fmap fabc

{-| Lifts a pure trifunction to operate on side-effect parameters -}
liftA3 : (a -> b -> c -> d) -> SE se a -> SE se b -> SE se c -> SE se d
liftA3 fabcd da = ap << liftA2 fabcd da

{-| Sequentially generates from a single side effect monad n times -}
repeated : SE se a -> Int -> SE se (List a)
repeated ga n = 
    if n <= 0 then return [] else 
        liftA2 (::) ga <| repeated ga (n - 1)
