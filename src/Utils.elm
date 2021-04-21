module Utils exposing (..)



unconsOrDie : String -> List a -> (a, List a)
unconsOrDie msg ls =
    case ls of
       [] -> Debug.todo <| "I died trying to uncons with: " ++ msg
       h :: t -> (h, t)

withNone : a -> (a, Cmd msg)
withNone x = (x, Cmd.none)

maybeAsList : Maybe a -> List a
maybeAsList = Maybe.withDefault [] << Maybe.map List.singleton

const : a -> b -> a
const x _ = x

maybe : b -> (a -> b) -> Maybe a -> b
maybe x f = Maybe.withDefault x << Maybe.map f

split : (a -> b) -> (a -> c) -> a -> (b, c)
split f g x = (f x, g x)

groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy f items =
    case items of
        [] -> []
        h :: t -> 
            let
                (eqis, neqis) = List.partition (f h) t
            in
                (h :: eqis) :: (if neqis == [] then [] else groupBy f neqis)

group : List a -> List (List a)
group = groupBy (==)

count : List a -> List (a, Int)
count = List.filterMap (\ls -> Maybe.map (\x -> (x, List.length ls)) (List.head ls)) << group