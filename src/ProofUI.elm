module ProofUI exposing
    ( NewLine
    , LineMsg(..)
    , renderLines
    , blank
    , updateNewLine
    , submitLine
    )

import Html exposing
    (Html, table, tr, td, th, input, select, option, text, div)
import Html.Attributes exposing
    (type_, value, hidden, selected, id, class, classList)
import Html.Events exposing (onInput)

import Proof exposing
    (Proof, DeductionRule(..), Deduction, showReason, addDeduction)
import WFF exposing (show)
import Parser exposing (parse)
import CustomSymbol exposing (makeMap)
import SequentUI exposing (selectSeq)
import SymbolUI exposing (selectSym)
import String exposing (join, toInt)
import List exposing (indexedMap, sort)

type alias NewLine =
    { formula : String
    , reason : Maybe DeductionRule
    , handleIndex : Int -> Maybe DeductionRule
    , indexing : Indexing
    , references : String
    }

blank : NewLine
blank =
    { formula = ""
    , reason = Just Assumption
    , handleIndex = always Nothing
    , indexing = NoIndexing
    , references = ""
    }

type LineMsg
    = Formula String
    | Reason String
    | ReasonIndex String
    | References String

type Indexing
    = SeqIndexing
    | SymIndexing
    | NoIndexing

updateNewLine : LineMsg -> NewLine -> NewLine
updateNewLine message oldline = case message of
    Formula s -> { oldline | formula = s }
    Reason s -> case s of
        "&E" -> simpleReason oldline AndElimination
        "&I" -> simpleReason oldline AndIntroduction
        "|E" -> simpleReason oldline OrElimination
        "|I" -> simpleReason oldline OrIntroduction
        "A" -> simpleReason oldline Assumption
        "CP" -> simpleReason oldline ConditionalProof
        "Def" ->
            { oldline
            | reason = Nothing
            , handleIndex = Just << Definition
            , indexing = SymIndexing
            }
        "DN" -> simpleReason oldline DoubleNegation
        "MP" -> simpleReason oldline ModusPonens
        "MT" -> simpleReason oldline ModusTollens
        "RAA" -> simpleReason oldline RAA
        "SI" ->
            { oldline
            | reason = Nothing
            , handleIndex = Just << Introduction
            , indexing = SeqIndexing
            }
        _ -> oldline
    ReasonIndex s -> case toInt s of
        Ok n -> { oldline | reason = oldline.handleIndex n }
        Err _ -> oldline
    References s -> { oldline | references = s }

{-  Given whether a reason index should be shown and the current line number,
    renders the new line input -}
renderNewLine : Indexing -> Proof -> Html LineMsg
renderNewLine indexing proof = tr [ id "new-line" ]
    [ td [ class "assumptions" ] []
    , td [ class "line-number" ]
        [ text <| "(" ++ toString ((List.length proof.lines)+1) ++ ")" ]
    , td [ class "formula" ]
        [ input [ type_ "text", onInput Formula, id "formula-input" ] [] ]
    , td [ class "reason" ]
        [ select [ onInput Reason, id "reason-dropdown" ]
            ( List.map (\x -> option [value x, selected (x=="A")] [text x])
                [ "&E"
                , "&I"
                , "|E"
                , "|I"
                , "A"
                , "CP"
                , "Def"
                , "DN"
                , "MP"
                , "MT"
                , "RAA"
                , "SI"
                ] )
        , div
            [ id "input-dropdown"
            , classList [("short", indexing /= NoIndexing)]
            ]
            [
                case indexing of
                    NoIndexing -> text ""
                    SymIndexing -> Html.map ReasonIndex <| selectSym proof
                    SeqIndexing -> Html.map ReasonIndex <| selectSeq proof
            ]
        , input
            [ type_ "text"
            , onInput References
            , id "reference-input"
            , classList
                [ ("long", indexing == NoIndexing)
                ]
            ] []
        ]
    ]

simpleReason : NewLine -> DeductionRule -> NewLine
simpleReason oldline reason =
    { oldline
    | reason = Just reason
    , handleIndex = always (Just reason)
    , indexing = NoIndexing
    }

-- Given the current proof, a deduction and its index, renders that deduction
renderDeduction : Proof -> Int -> Deduction -> Html LineMsg
renderDeduction proof index ded = tr []
    [ td [ class "assumptions" ]
        [ text <| join "," (List.map toString ded.assumptions) ]
    , td [ class "line-number" ] [ text <| "(" ++ toString (index+1) ++ ")" ]
    , td [ class "formula" ] [ text <| show ded.deduction ]
    , td [ class "reason" ] [ text <| showReason proof ded ]
    ]

lineHeading : Html LineMsg
lineHeading = tr []
    [ th [ class "assumptions" ] [ text "Assumptions" ]
    , th [ class "line-number" ] [ text "Line No." ]
    , th [ class "formula" ] [ text "Formula" ]
    , th [ class "reason" ] [ text "Reason" ]
    ]

-- Render the current proof as a table
renderLines : Proof -> NewLine -> Html LineMsg
renderLines proof newline = proof.lines
    |> indexedMap (renderDeduction proof)
    |> (::) lineHeading
    |> flip (++) [ renderNewLine newline.indexing proof ]
    |> table [ id "proof-lines" ]

-- Turns a list of results into result list, only if all were Ok
getAll : List (Result x a) -> Result x (List a)
getAll list = case list of
    [] -> Ok []
    (Err e)::_ -> Err e
    (Ok v)::ls -> Result.map ((::) v) (getAll ls)

-- Extracts numbers from a list that is seperated by commas and/or whitespace
extractNums : String -> Result String (List Int)
extractNums s = s
    |> String.map (\c -> if c == ',' then ' ' else c)
    |> String.words
    |> List.filter ((/=) "")
    |> List.map toInt
    |> getAll

-- Adds the current line to a proof
submitLine : Proof -> NewLine -> Result String Proof
submitLine proof newline = Result.map3 (Deduction [])
    (Result.mapError ((++) "Error in formula: ")
        <| parse (makeMap proof.symbols) newline.formula)
    (Result.fromMaybe "Incomplete reason" newline.reason)
    (extractNums newline.references
        |> Result.map (List.map (\x -> x-1))
        |> Result.map sort
        |> Result.mapError ((++) "Error in references: ")
    )
    |> Result.andThen (addDeduction proof)
