module UI
    ( run
    ) where

import Prelude
    (Unit, ($), (<>), (<<<), (*>), (<$>), bind, const, unit, pure)
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Effect.Ref as R
import Data.Maybe (Maybe(..))

import UI.AppState (AppStateM)
import UI.AppState as A
import UI.Sequent as UISeq
import UI.Symbol as UISym
import UI.Proof as UIP
import UI.Capabilities
    ( class ReadSymbols, class WriteSymbols, class ReadSequents, class ReadNav
    , class WriteSequents, class Nav, class Error, class ReadError
    , class WriteProof, class History, class ReadProof
    , Window(..)
    , getErrors, setWindow, undo, redo, new, clear, canNew, canUndo, canRedo
    )

data Action
    = Update
    | Open Window
    | Undo
    | Redo
    | New
    | ClearError
    | NoAction

type State =
    { errors :: Maybe (Array String)
    , ableNew :: Boolean
    , ableUndo :: Boolean
    , ableRedo :: Boolean
    }

type Slots =
    ( sequents :: UISeq.Slot Unit
    , symbols :: UISym.Slot Unit
    , proof :: UIP.Slot Unit
    )

_sequents :: SProxy "sequents"
_sequents = SProxy

_symbols :: SProxy "symbols"
_symbols = SProxy

_proof :: SProxy "proof"
_proof = SProxy

component :: forall q. H.Component HH.HTML q Action Unit AppStateM
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState =
    { errors : Nothing
    , ableNew : false
    , ableUndo : false
    , ableRedo : false
    }

menu :: forall m. State -> H.ComponentHTML Action Slots m
menu state = HH.div
    [ HP.id_ "menu" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableNew then
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "new-button"
        , HE.onClick $ const $ Just New
        ]
        [ HH.text "New" ]
    , HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableUndo then
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "undo-button"
        , HE.onClick $ const $ Just Undo
        ]
        [ HH.text "Undo" ]
    , HH.div
        [ HP.class_ $ HH.ClassName $
            if state.ableRedo then 
                "menu-button"
            else
                "menu-button disabled"
        , HP.id_ "redo-button"
        , HE.onClick $ const $ Just Redo
        ]
        [ HH.text "Redo" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "symbol-window-button"
        , HE.onClick $ const $ Just $ Open SymbolWindow
        ]
        [ HH.text "Symbols" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "sequent-window-button"
        , HE.onClick $ const $ Just $ Open SequentWindow
        ]
        [ HH.text "Sequents" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "menu-button"
        , HP.id_ "import-export-window-button"
        , HE.onClick $ const $ Just $ Open IEWindow
        ]
        [ HH.text "Import/Export" ]
    ]

render :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    ReadSequents m =>
    WriteSequents m =>
    ReadProof m =>
    WriteProof m =>
    Error m =>
    ReadNav m =>
    Nav m =>
    State -> H.ComponentHTML Action Slots m
render state = HH.div
    [ HP.id_ "main" ] $
    [ menu state
    , HH.slot _sequents unit UISeq.component UISeq.NoAction
        (const $ Just Update)
    , HH.slot _symbols unit UISym.component UISym.NoAction
        (const $ Just Update)
    , HH.slot _proof unit UIP.component UIP.NoAction
        (const $ Just Update)
    ] <> case state.errors of
        Nothing -> []
        Just errs ->
            [ HH.div
                [ HP.id_ "error"
                , HE.onClick $ const $ Just ClearError
                ]
                $ (\e -> HH.div [] [HH.text e]) <$> errs
            ]

handleAction :: forall m.
    Error m =>
    ReadError m =>
    Nav m =>
    History m =>
    Action -> H.HalogenM State Action Slots Unit m Unit
handleAction Update = do
    _ <- H.query _sequents unit $ UISeq.Update unit
    _ <- H.query _symbols unit $ UISym.Update unit
    _ <- H.query _proof unit $ UIP.Update unit
    errors <- getErrors
    ableNew <- canNew
    ableUndo <- canUndo
    ableRedo <- canRedo
    H.put {errors, ableNew, ableUndo, ableRedo}
handleAction (Open w) = setWindow w *> handleAction Update
handleAction Undo = undo *> handleAction Update
handleAction Redo = redo *> handleAction Update
handleAction New = new *> handleAction Update
handleAction ClearError = clear *> handleAction Update
handleAction NoAction = pure unit

run :: Effect Unit
run = HA.runHalogenAff do
    body <- HA.awaitBody
    ref <- H.liftEffect $ R.new A.start
    runUI (H.hoist (H.liftEffect <<< A.run ref) component) NoAction body
