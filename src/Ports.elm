port module Ports exposing (..)

import Json.Encode exposing (Value)

port storeProof : Value -> Cmd msg
