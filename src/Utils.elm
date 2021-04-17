module Utils exposing (..)

unconsOrDie : String -> List a -> (a, List a)
unconsOrDie msg ls =
    case ls of
       [] -> Debug.todo <| "I died trying to uncons with: " ++ msg
       h :: t -> (h, t)

withNone : a -> (a, Cmd msg)
withNone x = (x, Cmd.none)