module SymbolJson exposing
    ( tojson
    , fromjson
    , allfromjson
    )

import Json.Encode exposing (Value)
import Json.Encode as E
import Json.Decode exposing (Decoder)
import Json.Decode as D

import Parser exposing (SymbolMaps, parse)
import CustomSymbol exposing
    (Symbol, makeUnary, makeBinary, makeMap, augmentMap)
import WFF exposing (WFF(..))
import WFFJson

fromResult : Result String a -> Decoder a
fromResult x = case x of
    Ok v -> D.succeed v
    Err e -> D.fail e

tojson : Symbol -> Result String Value
tojson s = case s.wff of
    Prop _ -> Err "Error: symbol is a proposition"
    Unary u -> case u.contents of
        Prop a -> Ok <| E.object
            [ ("symbol", E.string u.symbol)
            , ("prop", E.string a)
            , ("definition", WFFJson.tojson s.definition)
            ]
        _ -> Err "Error: unary operator does not contain proposition"
    Binary v -> case (v.first, v.second) of
        (Prop a, Prop b) -> Ok <| E.object
            [ ("symbol", E.string v.symbol)
            , ("propa", E.string a)
            , ("propb", E.string b)
            , ("definition", WFFJson.tojson s.definition)
            ]
        _ -> Err "Error: binary operator does not contain propositions"

type SymbolDecoded
    = UnaryDecoded
        { symbol : String
        , prop : String
        , wff : String
        }
    | BinaryDecoded
        { symbol : String
        , propa : String
        , propb : String
        , wff : String
        }

toDecoded : Decoder SymbolDecoded
toDecoded = D.oneOf
    [ D.map3
        (\x -> \y -> \z ->
            UnaryDecoded { symbol = x, prop = y, wff = z} )
        (D.field "symbol" D.string)
        (D.field "prop" D.string)
        (D.field "definition" D.string)
    , D.map4
        (\w -> \x -> \y -> \z ->
            BinaryDecoded { symbol = w, propa = x, propb = y, wff = z} )
        (D.field "symbol" D.string)
        (D.field "propa" D.string)
        (D.field "propb" D.string)
        (D.field "definition" D.string)
    ]

fromDecoded : SymbolMaps -> SymbolDecoded -> Result String Symbol
fromDecoded maps decoded = case decoded of
    UnaryDecoded v -> parse maps v.wff
        |> Result.andThen (makeUnary v.prop v.symbol)
    BinaryDecoded v -> parse maps v.wff
        |> Result.andThen (makeBinary v.propa v.propb v.symbol)

fromjson : SymbolMaps -> Decoder Symbol
fromjson maps = D.map (fromDecoded maps) toDecoded |> D.andThen fromResult

allfromDecoded : List SymbolDecoded -> Result String (List Symbol)
allfromDecoded list = List.foldl
    ( \decoded -> \x -> case x of
        Err e -> Err e
        Ok (list, maps) -> case fromDecoded maps decoded of
            Err e -> Err e
            Ok newsymbol -> Ok (newsymbol::list, augmentMap newsymbol maps) )
    (Ok ([], makeMap []))
    list
    |> Result.map Tuple.first
    |> Result.map List.reverse

allfromjson : Decoder (List Symbol)
allfromjson = D.list toDecoded |> D.andThen (fromResult << allfromDecoded)
