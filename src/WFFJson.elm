module WFFJson exposing
    ( tojson
    , fromjson
    )

import Json.Encode exposing (Value)
import Json.Encode as E
import Json.Decode exposing (Decoder)
import Json.Decode as D

import Parser exposing (SymbolMaps, parse)
import WFF exposing (WFF)

tojson : WFF -> Value
tojson wff = E.string <| WFF.show wff

fromjson : SymbolMaps -> Decoder WFF
fromjson maps = D.string
    |> D.andThen (\wff -> case parse maps wff of
        Ok v -> D.succeed v
        Err e -> D.fail e
    )
