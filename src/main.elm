import Html exposing (Html, div, text, button)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)

import Proof exposing (Proof, DeductionRule(..), empty, addSequent)
import ProofUI exposing
    (NewLine, LineMsg(..), renderLines, blank, updateNewLine, submitLine)
import SequentUI exposing
    (NewSequent, SequentMsg, blank, updateSeq, renderSequents, submitSeq)

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
    }

start : Model
start =
    { proof = empty
    , history = []
    , latestError = Nothing
    , newLine = ProofUI.blank
    , newSeq = SequentUI.blank
    }

type Msg
    = Lines LineMsg
    | SubmitLine
    | NewSeq SequentMsg
    | AddSequent

update : Msg -> Model -> Model
update msg model = case msg of
    Lines linemsg -> { model | newLine = updateNewLine linemsg model.newLine }
    SubmitLine -> case submitLine model.proof model.newLine of
        Err s -> { model | latestError = Just s }
        Ok p ->
            { history = model.proof :: model.history
            , proof = p
            , latestError = Nothing
            , newLine = ProofUI.blank
            , newSeq = model.newSeq
            }
    NewSeq seqmsg -> { model | newSeq = updateSeq model.newSeq seqmsg }
    AddSequent -> case submitSeq model.proof model.newSeq of
        Err s -> { model | latestError = Just s }
        Ok n ->
            { history = model.proof :: model.history
            , proof = addSequent n model.proof
            , latestError = Nothing
            , newLine = model.newLine
            , newSeq = SequentUI.blank
            }

view : Model -> Html Msg
view model = div []
    [ Html.map NewSeq <| div [] (renderSequents model.proof model.newSeq)
    , button [ onClick AddSequent ] [ text "Add Sequent" ]
    , Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine ] [ text "Add Line" ]
    , case model.latestError of
        Nothing -> text ""
        Just e -> div [ id "Error" ] [ text e ]
    ]
