module UI.Symbol
    ( Action(..)
    , Message
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( (<$>), (<>), ($), (<<<), (<$)
    , Unit
    , const, otherwise, bind, discard, pure, unit
    )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))

import WFF as WFF
import UI.HTMLHelp (select)
import Parser (parseSymbol)
import Symbol (Symbol(..), makeUnary, makeBinary)
import UI.Capabilities
    ( class ReadSymbols, class WriteSymbols, class Nav, class Error
    , class ReadNav
    , parse, error, getSymbols, addSymbol, isSymbolWindow, close
    )

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = Name String
    | Def String
    | Binary Boolean
    | Add
    | Close
    | NoAction

type Message = Unit

type State =
    { symbols :: Array Symbol
    , open :: Boolean
    , name :: String
    , definition :: String
    , binary :: Boolean
    }

component :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    Nav m =>
    Error m =>
    ReadNav m =>
    H.Component HH.HTML Query Action Message m
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: State
initialState =
    { symbols : []
    , open : false
    , name : ""
    , definition : ""
    , binary : true
    }

symbolRow :: forall m. Symbol -> H.ComponentHTML Action () m
symbolRow (UnarySymbol s) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ s.operator.symbol <> "A" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ≡ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text $ WFF.render $ "A" <$ s.definition ]
    ]
symbolRow (BinarySymbol s) = HH.tr
    []
    [ HH.td [] []
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text $ "A" <> s.operator.symbol <> "B" ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ≡ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.text $ WFF.render $ (if _ then "A" else "B") <$> s.definition ]
    ]

newSymbolRow :: forall m. State -> H.ComponentHTML Action () m
newSymbolRow state = HH.tr
    [ HP.id_ "new-symbol" ]
    [ HH.td
        []
        [ select (Just <<< Binary) (const false) state.binary
            [ Tuple "Binary" true
            , Tuple "Unary" false
            ]
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-name" ]
        [ HH.text (if state.binary then "A" else "")
        , HH.input
            [ HE.onValueChange $ Just <<< Name
            , HP.value state.name
            , HP.id_ "choose-name"
            ] -- todo make input type text?
        , HH.text (if state.binary then "B" else "A")
        ]
    , HH.td
        [ HP.class_ $ HH.ClassName "equiv-symbol" ]
        [ HH.text " ≡ " ]
    , HH.td
        [ HP.class_ $ HH.ClassName "symbol-def" ]
        [ HH.input
            [ HE.onValueChange $ Just <<< Def
            , HP.value state.definition
            , HP.id_ "set-def"
            ] -- todo make input type text?
        ]
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state
    | state.open = HH.div
        [ HP.class_ $ HH.ClassName "floating"
        , HP.id_ "symbol-box"
        ]
        [ HH.table
            [ HP.id_ "symbol-list" ]
            $ (symbolRow <$> state.symbols) <> [ newSymbolRow state ]
        , HH.div
            [ HE.onClick $ const $ Just Add
            , HP.id_ "add-symbol"
            , HP.class_ $ HH.ClassName "button"
            ]
            [ HH.text "Add Symbol" ]
        , HH.div
            [ HE.onClick $ const $ Just Close
            , HP.id_ "close-button"
            , HP.class_ $ HH.ClassName "button"
            ]
            [ HH.text "Close" ]
        ]
    | otherwise = HH.div [] []

handleAction :: forall m.
    ReadSymbols m =>
    WriteSymbols m =>
    Nav m =>
    Error m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (Name n) = H.modify_ $ _ { name = n }
handleAction (Def d) = H.modify_ $ _ { definition = d }
handleAction (Binary b) = H.modify_ $ _ { binary = b }
handleAction Add = do
    state <- H.get
    defp <- parse state.definition
    case do
        name <- parseSymbol state.name
        def <- defp
        if state.binary then
            makeBinary "A" "B" name def
        else
            makeUnary "A" name def
    of
        Left e -> error e
        Right symbol -> do
            H.modify_ $ _ { name = "", definition = "", binary = true }
            addSymbol symbol
handleAction Close = close
handleAction NoAction = pure unit

handleQuery :: forall a m.
    ReadSymbols m =>
    ReadNav m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    symbols <- getSymbols
    open <- isSymbolWindow
    H.modify_ $ _ { symbols = symbols, open = open }
    pure $ Just a
