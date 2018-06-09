import Html exposing
    (Html, div, text, tr, td, table, input, button, select, option)
import Html.Attributes exposing (type_, value, disabled, selected)
import Html.Events exposing (onInput, onClick)

import Proof exposing (Proof, Deduction, DeductionRule(..), empty, showReason)
import WFF exposing (show)
import List exposing (indexedMap)
import String exposing (join, toInt)

main : Program Never Model Msg
main = Html.beginnerProgram
    { model = start
    , view = view
    , update = update
    }

type alias NewLine =
    { assumptions : String
    , formula : String
    , reason : Maybe DeductionRule
    , handleIndex : Int -> Maybe DeductionRule
    , enableIndex : Bool
    , references : String
    }

type alias Model =
    { proof : Proof
    , history : List Proof
    , latestError : Maybe String
    , newLine : NewLine
    }

start : Model
start =
    { proof = empty
    , history = []
    , latestError = Nothing
    , newLine = blank
    }

blank : NewLine
blank =
    { assumptions = ""
    , formula = ""
    , reason = Nothing
    , handleIndex = always Nothing
    , enableIndex = False
    , references = ""
    }

type Msg
    = UDassumptions String
    | UDwff String
    | UDReason String
    | UDReasonIndex String
    | UDReferences String
    | SubmitLine

update : Msg -> Model -> Model
update msg model = { model | newLine = updateNewLine msg model.newLine }

simpleReason : NewLine -> DeductionRule -> NewLine
simpleReason oldline reason =
    { oldline
    | reason = Just reason
    , handleIndex = always (Just reason)
    , enableIndex = False
    }

updateNewLine : Msg -> NewLine -> NewLine
updateNewLine message oldline = case message of
    UDassumptions s -> { oldline | assumptions = s }
    UDwff s ->      { oldline | formula = s }
    UDReason s -> case s of
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
            , enableIndex = True
            }
        "DN" -> simpleReason oldline DoubleNegation
        "MP" -> simpleReason oldline ModusPonens
        "MT" -> simpleReason oldline ModusTollens
        "RAA" -> simpleReason oldline RAA
        "SI" ->
            { oldline
            | reason = Nothing
            , handleIndex = Just << Introduction
            , enableIndex = True
            }
        _ -> oldline
    UDReasonIndex s -> case toInt s of
        Ok n -> { oldline | reason = oldline.handleIndex n }
        Err _ -> oldline
    _ -> oldline

newLine : Bool -> Int -> Html Msg
newLine allowIndex curIndex = tr []
    [ td [] [ input [ type_ "text", onInput UDassumptions ] [] ]
    , td [] [ text <| "(" ++ toString curIndex ++ ")" ]
    , td [] [ input [ type_ "text", onInput UDwff ] [] ]
    , td []
        [ select [ onInput UDReason ]
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
        , input
            [ type_ "number"
            , onInput UDReasonIndex
            , disabled (not allowIndex)
            ] []
        , input [ type_ "text", onInput UDReferences ] []
        ]
    ]

-- Given the current proof, a deduction and its index, renders that deduction
renderDeduction : Proof -> (Int, Deduction) -> Html Msg
renderDeduction proof (index, ded) = tr []
    [ td [] [ text <| join ", " (List.map toString ded.assumptions) ]
    , td [] [ text <| "(" ++ toString index ++ ")" ]
    , td [] [ text <| show ded.deduction ]
    , td [] [ text <| showReason proof ded ]
    ]

-- Render the current proof as a table
renderLines : Model -> Html Msg
renderLines model = model.proof.lines
    |> indexedMap (,)
    |> List.map (renderDeduction model.proof)
    |> flip (++) [ newLine
        (model.newLine.enableIndex)
        ((List.length model.proof.lines)+1) ]
    |> table []

view : Model -> Html Msg
view model = div []
    [ renderLines model
    , button [ onClick SubmitLine ] [ text "Add Line" ]
    ]
