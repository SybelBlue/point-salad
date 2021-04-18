module Message exposing (..)
import Vector3

type Msg 
    = Selected Vector3.Index (Maybe Bool)
