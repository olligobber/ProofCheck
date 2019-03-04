import Html exposing (Html, div, text, button, span, textarea)
import Html.Attributes exposing (id, class, disabled, classList, value)
import Html.Events exposing (onClick, onInput)

import Json.Encode exposing (encode, Value)
import Json.Decode exposing (decodeString, decodeValue)

import Proof exposing (Proof, DeductionRule(..), empty, addSequent, addSymbol)
import ProofUI exposing
    (NewLine, LineMsg(..), renderLines, blank, updateNewLine, submitLine)
import SequentUI exposing
    (NewSequent, SequentMsg, blank, updateSeq, renderSequents, submitSeq)
import SymbolUI exposing
    (NewSymbol, SymbolMsg, blank, updateSym, renderSymbols, submitSym)
import ProofJson exposing (..)
import Ports exposing (..)

main : Program (Maybe Value) Model Msg
main = Html.programWithFlags
    { init = start
    , view = view
    , update = update
    , subscriptions = always Sub.none
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
    , importText : Maybe String
    }

emptyModel : Model
emptyModel =
    { proof = empty
    , history = []
    , future = []
    , latestError = Nothing
    , newLine = ProofUI.blank
    , newSeq = SequentUI.blank
    , newSym = SymbolUI.blank
    , activeWindow = NoWindow
    , importText = Nothing
    }

start : Maybe Value -> (Model, Cmd Msg)
start flag = case flag of
    Nothing -> noMsg emptyModel
    Just val -> case decodeValue fromjson val of
        Err e -> noMsg
            { emptyModel
            | latestError = Just <| "Error loading previous proof: " ++ e
            }
        Ok proof -> noMsg
            { emptyModel
            | proof = proof
            }

type Msg
    = Lines LineMsg
    | SubmitLine
    | NewSeq SequentMsg
    | AddSequent
    | NewSym SymbolMsg
    | AddSymbol
    | New
    | Undo
    | Redo
    | Open Window
    | NewImport String
    | Import

type Window
    = NoWindow
    | SequentWindow
    | SymbolWindow
    | ImExWindow

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    Lines linemsg -> noMsg
        { model | newLine = updateNewLine linemsg model.newLine }
    SubmitLine -> case submitLine model.proof model.newLine of
        Err s -> noMsg { model | latestError = Just s }
        Ok p -> store
            { model
            | history = model.proof :: model.history
            , future = []
            , proof = p
            , latestError = Nothing
            , newLine = ProofUI.blank
            , importText = Nothing
            }
    NewSeq seqmsg -> noMsg { model | newSeq = updateSeq model.newSeq seqmsg }
    AddSequent -> case submitSeq model.proof model.newSeq of
        Err s -> noMsg { model | latestError = Just s }
        Ok n -> store
            { model
            | history = model.proof :: model.history
            , future = []
            , proof = addSequent n model.proof
            , latestError = Nothing
            , newSeq = SequentUI.blank
            , importText = Nothing
            }
    NewSym symmsg -> noMsg { model | newSym = updateSym model.newSym symmsg }
    AddSymbol -> case
            submitSym model.proof model.newSym
            |> Result.andThen (flip addSymbol model.proof)
        of
            Err s -> noMsg { model | latestError = Just s }
            Ok p -> store
                { model
                | history = model.proof :: model.history
                , future = []
                , proof = p
                , latestError = Nothing
                , newSym = SymbolUI.blank
                , importText = Nothing
                }
    New ->
        if model.proof == Proof.empty then
            noMsg model
        else
            noMsg { emptyModel | history = model.proof :: model.history }
    Undo -> case model.history of
        [] -> noMsg model
        (x::xs) -> store
            { emptyModel
            | history = xs
            , future = model.proof::model.future
            , proof = x
            , activeWindow = model.activeWindow
            }
    Redo -> case model.future of
        [] -> noMsg model
        (x::xs) -> store
            { emptyModel
            | history = model.proof::model.history
            , future = xs
            , proof = x
            , activeWindow = model.activeWindow
            }
    Open x ->
        if model.activeWindow == x then
            noMsg { model | activeWindow = NoWindow }
        else
            noMsg { model | activeWindow = x }
    NewImport s -> noMsg { model | importText = Just s }
    Import -> case model.importText of
        Nothing -> noMsg model
        Just s -> case decodeString fromjson s of
            Err e -> noMsg
                { model
                | latestError = Just <| "Error importing proof: " ++ e
                }
            Ok proof -> store
                { emptyModel
                | history = model.proof :: model.history
                , proof = proof
                }

closeButton : Html Msg
closeButton = div
    [ onClick <| Open NoWindow
    , id "close-button"
    , class "button"
    ] [ text "Close" ]

sequentBox : Model -> Html Msg
sequentBox model = div [ class "floating", id "sequent-box" ]
    [ Html.map NewSeq <| renderSequents model.proof model.newSeq
    , div [ onClick AddSequent, id "add-sequent", class "button" ] [ text "Add Sequent" ]
    , closeButton
    ]

symbolBox : Model -> Html Msg
symbolBox model = div [ class "floating", id "symbol-box" ]
    [ Html.map NewSym <| renderSymbols model.proof model.newSym
    , div [ onClick AddSymbol, id "add-symbol", class "button" ] [ text "Add Symbol" ]
    , closeButton
    ]

imexText : Model -> String
imexText model = case (tojson model.proof, model.importText) of
    (_, Just s) -> s
    (Err e, Nothing) -> "Error exporting proof: " ++ e
    (Ok json, Nothing) -> encode 0 json

imexBox : Model -> Html Msg
imexBox model = div [ class "floating", id "import-export-box" ]
    [ div [ id "i-e-description" ]
        [ text
            "Copy box contents to save proof, or paste inside the box to import proof"
        ]
    , div [ id "json-input" ]
        [ textarea
            [ onInput NewImport
            , value <| imexText model
            ] []
        ]
    , button
        [ onClick Import
        , id "import-button"
        , classList
            [ ("button", True)
            , ("disabled", model.importText == Nothing)
            ]
        , disabled <| model.importText == Nothing
        ] [ text "Import" ]
    , closeButton
    ]

activeBox : Model -> Html Msg
activeBox model = case model.activeWindow of
    NoWindow -> text ""
    SequentWindow -> sequentBox model
    SymbolWindow -> symbolBox model
    ImExWindow -> imexBox model

proofBox : Model -> Html Msg
proofBox model = div [ id "proof-box" ]
    [ Html.map Lines <| renderLines model.proof model.newLine
    , button [ onClick SubmitLine, id "add-line" ] [ text "Add Line" ]
    ]

menu : Model -> Html Msg
menu model = div [ id "menu" ]
    [ div
        [ classList
            [ ("menu-button", True)
            , ("disabled", model.proof == Proof.empty)
            ]
        , id "new-button"
        , onClick New
        ] [ text "New" ]
    , div
        [ classList
            [ ("menu-button", True)
            , ("disabled", model.history == [])
            ]
        , id "undo-button"
        , onClick Undo
        ] [ text "Undo" ]
    , div
        [ classList
            [ ("menu-button", True)
            , ("disabled", model.future == [])
            ]
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
    , div
        [ class "menu-button"
        , id "import-export-window-button"
        , onClick <| Open ImExWindow
        ] [ text "Import/Export" ]
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

store : Model -> (Model, Cmd.Cmd Msg)
store model = case tojson model.proof of
    Err e -> (model, Cmd.none) -- Display an error?
    Ok val -> (model, storeProof val)

noMsg : Model -> (Model, Cmd.Cmd msg)
noMsg model = (model, Cmd.none)
