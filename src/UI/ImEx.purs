module UI.ImEx
    ( Action(..)
    , Message
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude (Unit, const, ($), (<<<), bind, pure, unit, discard)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.MediaType.Common (applicationJSON)
import Web.Event.Event (Event)

import UI.Capabilities
    (class Nav, class ReadNav, class ReadFile, readFile, close, isIEWindow)

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = Close
    | Load Event
    | NoAction

type Message = Unit

type State =
    { open :: Boolean
    }

component :: forall m.
    Nav m =>
    ReadNav m =>
    ReadFile m =>
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
    { open : false
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state | state.open = HH.div
    [ HP.class_ $ H.ClassName "floating"
    , HP.id_ "import-export-box"
    ]
    [ HH.div
        [ HP.id_ "json-input" ]
        [ HH.input
            [ HE.onInput $ Just <<< Load
            , HP.type_ HP.InputFile
            , HP.accept $ mediaType applicationJSON
            ]
        ]
    , HH.div
        [ HE.onClick $ const $ Just Close
        , HP.id_ "close-button"
        , HP.class_ $ HH.ClassName "button"
        ]
        [ HH.text "Close" ]
    ]
render _ = HH.div [] []

handleAction :: forall m.
    Nav m =>
    ReadFile m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction Close = close
handleAction (Load e) = readFile e
handleAction NoAction = pure unit

handleQuery :: forall a m.
    ReadNav m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    open <- isIEWindow
    H.put {open}
    pure $ Just a
