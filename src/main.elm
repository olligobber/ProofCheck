import Html exposing (Html, div, text, button)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)

import Proof exposing (Proof, DeductionRule(..), empty, addSequent)
import ProofLines exposing
    (NewLine, LineMsg(..), renderLines, blank, updateNewLine, submitLine)
import NewSequent exposing
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
    , newLine = ProofLines.blank
    , newSeq = NewSequent.blank
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
            , newLine = ProofLines.blank
            , newSeq = model.newSeq
            }
    NewSeq seqmsg -> case updateSeq model.proof model.newSeq seqmsg of
        Err s -> { model | latestError = Just s }
        Ok n -> { model | latestError = Nothing, newSeq = n }
    AddSequent -> case submitSeq model.proof model.newSeq of
        Err s -> { model | latestError = Just s }
        Ok n ->
            { history = model.proof :: model.history
            , proof = addSequent n model.proof
            , latestError = Nothing
            , newLine = model.newLine
            , newSeq = NewSequent.blank
            }

view : Model -> Html Msg
view model = div []
    [ div [] <| List.map (Html.map NewSeq) (renderSequents model.proof model.newSeq)
    , button [ onClick AddSequent ] [ text "Add Sequent" ]
    , Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine ] [ text "Add Line" ]
    , case model.latestError of
        Nothing -> div [] []
        Just e -> div [ id "Error" ] [ text e ]
    ]
