module SymbolUI exposing
    ( NewSymbol
    , SymbolMsg
    , blank
    , updateSym
    , renderSymbols
    , submitSym
    , selectSym
    )

import Html exposing (Html, table, tr, td, text, input, div, option, select)
import Html.Attributes exposing (type_, value, id, selected, disabled)
import Html.Events exposing (onInput)

import WFF exposing (show)
import CustomSymbol exposing (Symbol, makeUnary, makeBinary, makeMap)
import Parser exposing (isSymbol, parse)
import Proof exposing (Proof)
import String exposing (filter)
import List exposing (indexedMap)

type alias NewSymbol =
    { name : String
    , definition : String
    , binary : Bool
    }

blank : NewSymbol
blank =
    { name = ""
    , definition = ""
    , binary = True
    }

type SymbolMsg
    = Name String
    | Def String
    | Binary String

updateSym : NewSymbol -> SymbolMsg -> NewSymbol
updateSym old msg = case msg of
    Name s -> { old | name = filter isSymbol s }
    Def s -> { old | definition = s }
    Binary s ->
        if s == "B" then
            { old | binary = True }
        else
            { old | binary = False }

renderNewSym : NewSymbol -> Html SymbolMsg
renderNewSym new = div [ id "NewSymbol" ]
    [ select [ onInput Binary ]
        [ option [ value "B", selected new.binary ] [ text "Binary" ]
        , option [ value "U", selected (not new.binary) ] [ text "Unary" ]
        ]
    , text (if new.binary then "A" else "")
    , input [ type_ "text", onInput Name, value new.name ] []
    , text (if new.binary then "B" else "A")
    , text " ≡ "
    , input [ type_ "text", onInput Def, value new.definition ] []
    ]

renderSymbols : Proof -> NewSymbol -> List (Html SymbolMsg)
renderSymbols proof new = proof.symbols
    |> List.map (\s -> tr []
        [ td [] [text (show s.wff)]
        , td [] [text " ≡ "]
        , td [] [text (show s.definition)]
        ] )
    |> table []
    |> flip (::) [renderNewSym new]

submitSym : Proof -> NewSymbol -> Result String Symbol
submitSym proof new = case
    ( new.binary
    , filter isSymbol new.name
    , parse (makeMap proof.symbols) new.definition
    ) of
        (_,_,Err e) -> Err e
        (False, n, Ok d) -> makeUnary "A" n d
        (True, n, Ok d) -> makeBinary "A" "B" n d

selectSym : (String -> msg) -> Proof -> Html msg
selectSym f proof = List.map .name proof.symbols
    |> indexedMap (\i -> \s -> option [value (toString i)] [text s])
    |> (::) (option [disabled True, selected True] [text "Choose One"])
    |> select [onInput f]
