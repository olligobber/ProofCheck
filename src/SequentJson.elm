module SequentJson exposing
    ( tojson
    , fromjson
    )

import Json.Encode exposing (Value)
import Json.Encode as E
import Json.Decode exposing (Decoder)
import Json.Decode as D

import Sequent exposing (Sequent)
import WFFJson
import Parser exposing (SymbolMaps)

tojson : Sequent -> Value
tojson s = E.object
    [ ("ante", E.list <| List.map WFFJson.tojson s.ante)
    , ("conse", WFFJson.tojson s.conse)
    ]

fromjson : SymbolMaps -> Decoder Sequent
fromjson maps = D.map2 Sequent
    (D.field "ante" <| D.list <| WFFJson.fromjson maps)
    (D.field "conse" <| WFFJson.fromjson maps)
