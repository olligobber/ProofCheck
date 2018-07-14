module ProofLines exposing
    ( NewLine
    , LineMsg(..)
    , renderLines
    , blank
    , updateNewLine
    , submitLine
    )

import Html exposing (Html, table, tr, td, input, select, option, text, div)
import Html.Attributes exposing (type_, value, hidden, selected, id)
import Html.Events exposing (onInput)

import Proof exposing
    (Proof, DeductionRule(..), Deduction, showReason, addDeduction)
import WFF exposing (show)
import Parser exposing (parse)
import CustomSymbol exposing (makeMap)
import String exposing (join, toInt)
import List exposing (indexedMap, sort)
import List.Extra exposing (unique)

type alias NewLine =
    { formula : String
    , reason : Maybe DeductionRule
    , handleIndex : Int -> Maybe DeductionRule
    , enableIndex : Bool
    , references : String
    }

blank : NewLine
blank =
    { formula = ""
    , reason = Just Assumption
    , handleIndex = always Nothing
    , enableIndex = False
    , references = ""
    }

type LineMsg
    = Formula String
    | Reason String
    | ReasonIndex String
    | References String

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
            , handleIndex = Just << Definition << (\x -> x-1)
            , enableIndex = True
            }
        "DN" -> simpleReason oldline DoubleNegation
        "MP" -> simpleReason oldline ModusPonens
        "MT" -> simpleReason oldline ModusTollens
        "RAA" -> simpleReason oldline RAA
        "SI" ->
            { oldline
            | reason = Nothing
            , handleIndex = Just << Introduction << (\x -> x-1)
            , enableIndex = True
            }
        _ -> oldline
    ReasonIndex s -> case toInt s of
        Ok n -> { oldline | reason = oldline.handleIndex n }
        Err _ -> oldline
    References s -> { oldline | references = s }

{-  Given whether a reason index should be shown and the current line number,
    renders the new line input -}
renderNewLine : Bool -> Int -> Html LineMsg
renderNewLine allowIndex curIndex = tr []
    [ td [] []
    , td [] [ text <| "(" ++ toString curIndex ++ ")" ]
    , td [] [ input [ type_ "text", onInput Formula ] [] ]
    , td []
        [ select [ onInput Reason ]
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
        , if allowIndex
            then
                input
                    [ type_ "number"
                    , onInput ReasonIndex
                    ] []
            else
                div [] []
        , input [ type_ "text", onInput References ] []
        ]
    ]

simpleReason : NewLine -> DeductionRule -> NewLine
simpleReason oldline reason =
    { oldline
    | reason = Just reason
    , handleIndex = always (Just reason)
    , enableIndex = False
    }

-- Given the current proof, a deduction and its index, renders that deduction
renderDeduction : Proof -> (Int, Deduction) -> Html LineMsg
renderDeduction proof (index, ded) = tr []
    [ td [] [ text <| join "," (List.map toString ded.assumptions) ]
    , td [] [ text <| "(" ++ toString (index+1) ++ ")" ]
    , td [] [ text <| show ded.deduction ]
    , td [] [ text <| showReason proof ded ]
    ]

-- Render the current proof as a table
renderLines : Proof -> NewLine -> Html LineMsg
renderLines proof newline = proof.lines
    |> indexedMap (,)
    |> List.map (renderDeduction proof)
    |> flip (++) [ renderNewLine
        (newline.enableIndex)
        ((List.length proof.lines)+1) ]
    |> table [ id "ProofLines" ]

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
