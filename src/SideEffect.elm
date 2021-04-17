module SideEffect exposing (..)
import Tuple exposing (mapFirst, pair)
import Basics.Extra exposing (uncurry)

{-| A monadic generic side effect isolator -}
type SE se res = SE (se -> (res, se))

{-| Unwraps a side-effect object -}
run : SE se res -> se -> (res, se)
run (SE f) = f

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
