module Draw exposing (..)

import Card exposing (..)
import Random exposing (Gen(..), Seed, run)
import Basics.Extra exposing (uncurry, flip)
import Tuple exposing (mapFirst, first, pair)
import Random
import Html exposing (a)


type Draw a = Draw (List Card -> Gen (a, List Card))

using : Draw a -> List Card -> Gen (a, List Card)
using (Draw da) = da

runUsing : Draw a -> List Card -> Seed -> ((a, List Card), Seed)
runUsing da cards = run (using da cards)

pick : Draw a -> List Card -> Seed -> a
pick da cards = first << first << run (using da cards)

bind : Draw a -> (a -> Draw b) -> Draw b
bind da fadb = Draw (\cards -> Gen (uncurry run << mapFirst (uncurry (using << fadb)) << run (using da cards)))

fmap : (a -> b) -> Draw a -> Draw b
fmap fab da = Draw (Random.fmap (mapFirst fab) << using da)

ap : Draw (a -> b) -> Draw a -> Draw b
ap dfab da = Draw (\cards ->
        Gen (\seed -> 
            let
                ((fab, cs), nseed) = runUsing dfab cards seed
            in
                mapFirst (mapFirst fab) <| run (using da cs) nseed
        )
    )

return : a -> Draw a
return x = Draw (\cards -> Random.fmap (flip pair cards) <| Random.return x)

liftA2 : (a -> b -> c) -> Draw a -> Draw b -> Draw c
liftA2 fabc = ap << fmap fabc

liftA3 : (a -> b -> c -> d) -> Draw a -> Draw b -> Draw c -> Draw d
liftA3 fabcd da = ap << liftA2 fabcd da