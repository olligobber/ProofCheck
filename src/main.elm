import Html exposing (Html, div, text, button, span)
import Html.Attributes exposing (id, class, disabled)
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
    , future : List Proof
    , latestError : Maybe String
    , newLine : NewLine
    , newSeq : NewSequent
    , newSym : NewSymbol
    , activeWindow : Window
    }

start : Model
start =
    { proof = empty
    , history = []
    , future = []
    , latestError = Nothing
    , newLine = ProofUI.blank
    , newSeq = SequentUI.blank
    , newSym = SymbolUI.blank
    , activeWindow = NoWindow
    }

type Msg
    = Lines LineMsg
    | SubmitLine
    | NewSeq SequentMsg
    | AddSequent
    | NewSym SymbolMsg
    | AddSymbol
    | Undo
    | Redo
    | Open Window

type Window
    = NoWindow
    | SequentWindow
    | SymbolWindow

update : Msg -> Model -> Model
update msg model = case msg of
    Lines linemsg -> { model | newLine = updateNewLine linemsg model.newLine }
    SubmitLine -> case submitLine model.proof model.newLine of
        Err s -> { model | latestError = Just s }
        Ok p ->
            { model
            | history = model.proof :: model.history
            , future = []
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
            , future = []
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
            , future = []
            , proof = addSymbol n model.proof
            , latestError = Nothing
            , newSym = SymbolUI.blank
            }
    Undo -> case model.history of
        [] -> model
        (x::xs) ->
            { model
            | history = xs
            , future = model.proof::model.future
            , proof = x
            }
    Redo -> case model.future of
        [] -> model
        (x::xs) ->
            { model
            | history = model.proof::model.history
            , future = xs
            , proof = x
            }
    Open x -> { model | activeWindow = x }

closeButton : Html Msg
closeButton = button
    [ onClick <| Open NoWindow
    , id "close-button"
    ] [ text "X" ]

sequentBox : Model -> Html Msg
sequentBox model = div [ class "floating", id "sequent-box" ]
    [ Html.map NewSeq <| renderSequents model.proof model.newSeq
    , button [ onClick AddSequent, id "add-sequent" ] [ text "Add Sequent" ]
    , closeButton
    ]

symbolBox : Model -> Html Msg
symbolBox model = div [ class "floating", id "symbol-box" ]
    [ Html.map NewSym <| renderSymbols model.proof model.newSym
    , button [ onClick AddSymbol, id "add-symbol" ] [ text "Add Symbol" ]
    , closeButton
    ]

activeBox : Model -> Html Msg
activeBox model = case model.activeWindow of
    NoWindow -> text ""
    SequentWindow -> sequentBox model
    SymbolWindow -> symbolBox model

proofBox : Model -> Html Msg
proofBox model = div [ id "proof-box" ]
    [ Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine, id "add-line" ] [ text "Add Line" ]
    ]

menu : Model -> Html Msg
menu model = div [ id "menu" ]
    [ span [ class "menu-heading" ] [ text "MENU" ]
    , div
        [ class <|
            if model.history == [] then
                "menu-button-disabled"
            else
                "menu-button"
        , id "undo-button"
        , onClick Undo
        ] [ text "Undo" ]
    , div
        [ class <|
            if model.future == [] then
                "menu-button-disabled"
            else
                "menu-button"
        , id "redo-button"
        , onClick Redo
        ] [ text "Redo" ]
    , div
        [ class "menu-button"
        , id "symbol-window-button"
        , onClick <| Open SymbolWindow
        ] [ text "Symbols" ]
    , div
        [ class "menu-button"
        , id "sequent-window-button"
        , onClick <| Open SequentWindow
        ] [ text "Sequents" ]
    ]

view : Model -> Html Msg
view model = div [ id "main" ]
    [ menu model
    , activeBox model
    , proofBox model
    , case model.latestError of
        Nothing -> text ""
        Just e -> div [ id "error" ] [ text e ]
    ]
