import Html exposing (Html, div, text, button)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)

import Proof exposing (Proof, DeductionRule(..), empty)
import ProofLines exposing
    (NewLine, LineMsg(..), renderLines, blank, updateNewLine, submitLine)

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
    }

start : Model
start =
    { proof = empty
    , history = []
    , latestError = Nothing
    , newLine = blank
    }

type Msg
    = Lines LineMsg
    | SubmitLine

update : Msg -> Model -> Model
update msg model = case msg of
    Lines linemsg -> { model | newLine = updateNewLine linemsg model.newLine }
    SubmitLine -> case submitLine model.proof model.newLine of
        Err s -> { model | latestError = Just s }
        Ok p ->
            { history = model.proof :: model.history
            , proof = p
            , latestError = Nothing
            , newLine = blank
            }

view : Model -> Html Msg
view model = div []
    [ Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine ] [ text "Add Line" ]
    , case model.latestError of
        Nothing -> div [] []
        Just e -> div [ id "Error" ] [ text e ]
    ]
