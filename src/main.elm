import Html exposing (Html, div, text, button)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)

import Proof exposing (Proof, DeductionRule(..), empty, addSequent, addSymbol)
import ProofUI exposing
    (NewLine, LineMsg(..), renderLines, blank, updateNewLine, submitLine)
import SequentUI exposing
    (NewSequent, SequentMsg, blank, updateSeq, renderSequents, submitSeq)
import SymbolUI exposing
    (NewSymbol, SymbolMsg, blank, updateSym, renderSymbols, submitSym)

main : Program Never Model Msg
main = Html.beginnerProgram
    { model = start
    , view = view
    , update = update
    }

type alias Model =
    { proof : Proof
    , history : List Proof
    , latestError : Maybe String
    , newLine : NewLine
    , newSeq : NewSequent
    , newSym : NewSymbol
    }

start : Model
start =
    { proof = empty
    , history = []
    , latestError = Nothing
    , newLine = ProofUI.blank
    , newSeq = SequentUI.blank
    , newSym = SymbolUI.blank
    }

type Msg
    = Lines LineMsg
    | SubmitLine
    | NewSeq SequentMsg
    | AddSequent
    | NewSym SymbolMsg
    | AddSymbol

update : Msg -> Model -> Model
update msg model = case msg of
    Lines linemsg -> { model | newLine = updateNewLine linemsg model.newLine }
    SubmitLine -> case submitLine model.proof model.newLine of
        Err s -> { model | latestError = Just s }
        Ok p ->
            { model
            | history = model.proof :: model.history
            , proof = p
            , latestError = Nothing
            , newLine = ProofUI.blank
            }
    NewSeq seqmsg -> { model | newSeq = updateSeq model.newSeq seqmsg }
    AddSequent -> case submitSeq model.proof model.newSeq of
        Err s -> { model | latestError = Just s }
        Ok n ->
            { model
            | history = model.proof :: model.history
            , proof = addSequent n model.proof
            , latestError = Nothing
            , newSeq = SequentUI.blank
            }
    NewSym symmsg -> { model | newSym = updateSym model.newSym symmsg }
    AddSymbol -> case submitSym model.proof model.newSym of
        Err s -> { model | latestError = Just s }
        Ok n ->
            { model
            | history = model.proof :: model.history
            , proof = addSymbol n model.proof
            , latestError = Nothing
            , newSym = SymbolUI.blank
            }

view : Model -> Html Msg
view model = div []
    [ Html.map NewSeq <| div [] (renderSequents model.proof model.newSeq)
    , button [ onClick AddSequent ] [ text "Add Sequent" ]
    , Html.map NewSym <| div [] (renderSymbols model.proof model.newSym)
    , button [ onClick AddSymbol ] [ text "Add Symbol" ]
    , Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine ] [ text "Add Line" ]
    , case model.latestError of
        Nothing -> text ""
        Just e -> div [ id "Error" ] [ text e ]
    ]
