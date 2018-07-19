module ProofJson exposing
    ( tojson
    -- , fromjson
    )

import Json.Encode exposing (Value)
import Json.Encode as E
import Json.Decode exposing (Decoder)
import Json.Decode as D

import Proof exposing (DeductionRule(..), Deduction, Proof, addAll, empty)
import WFFJson
import SymbolJson
import SequentJson
import Parser exposing (SymbolMaps)
import CustomSymbol exposing (makeMap)

fromResult : Result String a -> Decoder a
fromResult x = case x of
    Ok v -> D.succeed v
    Err e -> D.fail e

ruletojson : DeductionRule -> Value
ruletojson rule = case rule of
    Assumption -> E.string "A"
    ModusPonens -> E.string "MP"
    ModusTollens -> E.string "MT"
    DoubleNegation -> E.string "DN"
    ConditionalProof -> E.string "CP"
    AndIntroduction -> E.string "&I"
    AndElimination -> E.string "&E"
    OrIntroduction -> E.string "|I"
    OrElimination -> E.string "|E"
    RAA -> E.string "RAA"
    Definition i -> E.object
        [ ("rule", E.string "Def")
        , ("number", E.int i)
        ]
    Introduction i -> E.object
        [ ("rule", E.string "SI")
        , ("number", E.int i)
        ]

mustBe : a -> b -> Decoder a -> Decoder b
mustBe expected result = D.andThen ( \val ->
    if val == expected then
        D.succeed result
    else
        D.fail "Unexpected value" )

rulefromjson : Decoder DeductionRule
rulefromjson = D.oneOf
    [ mustBe "A" Assumption D.string
    , mustBe "MP" ModusPonens D.string
    , mustBe "MT" ModusTollens D.string
    , mustBe "DN" DoubleNegation D.string
    , mustBe "CP" ConditionalProof D.string
    , mustBe "&I" AndIntroduction D.string
    , mustBe "&E" AndElimination D.string
    , mustBe "|I" OrIntroduction D.string
    , mustBe "|E" OrElimination D.string
    , mustBe "RAA" RAA D.string
    , D.map2
        (<|)
        (D.oneOf
            [ mustBe "Def" Definition <| D.field "rule" D.string
            , mustBe "SI" Introduction <| D.field "rule" D.string
            ]
        )
        (D.field "number" D.int)
    ]

deductiontojson : Deduction -> Value
deductiontojson deduction = E.object
    [ ("formula", WFFJson.tojson deduction.deduction)
    , ("rule", ruletojson deduction.rule)
    , ("references", E.list <| List.map E.int deduction.reasons)
    ]

deductionfromjson : SymbolMaps -> Decoder Deduction
deductionfromjson maps = D.map3 (Deduction [])
    (D.field "formula" <| WFFJson.fromjson maps)
    (D.field "rule" rulefromjson)
    (D.field "references" <| D.list D.int)

-- Turns a list of results into result list, only if all were Ok
getAll : List (Result x a) -> Result x (List a)
getAll list = case list of
    [] -> Ok []
    (Err e)::_ -> Err e
    (Ok v)::ls -> Result.map ((::) v) (getAll ls)

tojson : Proof -> Result String Value
tojson proof = case getAll (List.map SymbolJson.tojson proof.symbols) of
    Err e -> Err e
    Ok list -> Ok <| E.object
        [ ("symbols", E.list list)
        , ("sequents", E.list <| List.map SequentJson.tojson proof.sequents)
        , ("lines", E.list <| List.map deductiontojson proof.lines)
        ]

fromjson : Decoder Proof
fromjson = D.field "symbols" SymbolJson.allfromjson
    |> D.andThen (\symbols ->
        let maps = makeMap symbols in
        D.map2
            (\x -> \y -> addAll symbols x y empty)
            (D.field "sequents" <| D.list (SequentJson.fromjson maps))
            (D.field "lines" <| D.list (deductionfromjson maps))
        |> D.andThen fromResult
    )
