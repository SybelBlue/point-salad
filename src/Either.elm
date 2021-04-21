module Either exposing ( .. )

import Utils exposing ( const )

type Either a b
    = Left a
    | Right b

mapFirst : (a -> b) -> Either a c -> Either b c
mapFirst f e =
    case e of
       Left x -> Left (f x)
       Right x -> Right x

mapSecond : (a -> b) -> Either c a -> Either c b
mapSecond f e =
    case e of
       Left x -> Left x
       Right x -> Right (f x)

mapEither : (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f g e =
    case e of
       Left x -> Left (f x)
       Right x -> Right (g x)

either : (a -> b) -> (c -> b) -> Either a c -> b
either f g e = 
    case e of
       Left x -> f x
       Right x -> g x

toMaybe : Either a b -> Maybe a
toMaybe = either Just (const Nothing)

isLeft : Either a b -> Bool
isLeft e = 
    case e of
       Left _  -> True
       Right _ -> False

isRight : Either a b -> Bool
isRight = not << isLeft
