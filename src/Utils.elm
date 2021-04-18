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