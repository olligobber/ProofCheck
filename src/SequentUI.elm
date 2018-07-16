module SequentUI exposing
    ( NewSequent
    , SequentMsg
    , blank
    , updateSeq
    , renderSequents
    , submitSeq
    , selectSeq
    )

import Html exposing (Html, table, tr, td, text, input, div, option, select)
import Html.Attributes exposing (type_, value, id, disabled, selected)
import Html.Events exposing (onInput)

import WFF exposing (WFF, show)
import Sequent exposing (Sequent, show)
import Proof exposing (Proof)
import String exposing (join, split)
import Parser exposing (parse)
import CustomSymbol exposing (makeMap)
import List exposing (indexedMap)
import String exposing (toInt)

type alias NewSequent =
    { ante : String
    , conse : String
    }

type SequentMsg
    = Ante String
    | Conse String

blank : NewSequent
blank =
    { ante = ""
    , conse = ""
    }

updateSeq : NewSequent -> SequentMsg -> NewSequent
updateSeq old msg = case msg of
    Ante s -> { old | ante = s }
    Conse s -> { old | conse = s }

renderNewSeq : NewSequent -> Html SequentMsg
renderNewSeq new = div [ id "NewSequent" ]
        [ input [ type_ "text", onInput Ante, value new.ante ] []
        , text " ⊢ "
        , input [ type_ "text", onInput Conse, value new.conse ] []
        ]

renderSequents : Proof -> NewSequent -> List (Html SequentMsg)
renderSequents proof new = proof.sequents
    |> List.map Sequent.show
    |> List.map (\s -> tr []
        [ td [] [ text s ] -- TODO improve alignment
        ] )
    |> table [ id "Sequents" ]
    |> flip (::) [renderNewSeq new]

-- Folds a list of results, returning the first error and its index
foldError : List (Result a b) -> Result (a, Int) (List b)
foldError = foldErrorIndex 1

foldErrorIndex : Int -> List (Result a b) -> Result (a, Int) (List b)
foldErrorIndex i list = case list of
    [] -> Ok []
    (Ok x)::rest -> Result.map ((::) x) <| foldErrorIndex (i+1) rest
    (Err e)::_ -> Err (e,i)

extractAssumptions : Proof -> String -> Result String (List WFF)
extractAssumptions proof s = split "," s
    |> List.map (parse <| makeMap proof.symbols)
    |> foldError
    |> Result.mapError ( \(s,i) -> s ++ " in assumption " ++ (toString i) )
    |> ( \a -> case (a,split "," s) of
        (Ok x, _) -> Ok x
        (Err "Parse Error: Empty Input in assumption 1", [_]) -> Ok []
        (Err e, _) -> Err e
    )

submitSeq : Proof -> NewSequent -> Result String Sequent
submitSeq proof new = case
    ( extractAssumptions proof new.ante
    , parse (makeMap proof.symbols) new.conse
    ) of
        (Err e, _) -> Err e
        (_, Err f) -> Err <| f ++ " in conclusion"
        (Ok a, Ok c) -> Ok { ante = a, conse = c }

selectSeq : (String -> msg) -> Proof -> Html msg
selectSeq f proof = List.map Sequent.show proof.sequents
        |> indexedMap (\i -> \s -> option [value (toString i)] [text s])
        |> (::) (option [disabled True, selected True] [text "Choose One"])
        |> select [onInput f]
