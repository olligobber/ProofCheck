module UI.AppState
    ( ProofState
    , AppState
    , AppStateM
    , run
    , start
    ) where

import Prelude
    ( class Functor, class Apply, class Applicative, class Bind, class Monad
    , ($), (>>=), (<$>), (<>)
    , bind, const, pure, unit
    , Unit
    )
import Data.Maybe (Maybe(..))
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask, lift)
import Effect (Effect)
import Effect.Ref (Ref, read, modify_)
import Data.Either (Either(..))
import Data.Array as A

import Sequent (Sequent)
import Symbol (Symbol, SymbolMap, defaultMap, updateMap)
import Proof (Proof)
import Proof as P
import UI.Capabilities
    ( Window(..)
    , class ReadSymbols, class WriteSymbols, class ReadSequents
    , class WriteSequents, class ReadProof, class WriteProof, class Error
    , class ReadError, class ReadNav, class Nav, class History
    , error
    )

type ProofState =
    { sequents :: Array (Sequent String)
    , symbols :: Array Symbol
    , symbolMap :: SymbolMap
    , proof :: Proof
    }

type AppState =
    { history :: Array ProofState
    , future :: Array ProofState
    , present :: ProofState
    , error :: Maybe (Array String)
    , window :: Window
    }

start :: AppState
start =
    { history : []
    , future : []
    , present :
        { sequents : []
        , symbols : []
        , symbolMap : defaultMap
        , proof : P.empty
        }
    , error : Nothing
    , window : NoWindow
    }

newtype AppStateM x = AppStateM (ReaderT (Ref AppState) Effect x)

derive newtype instance functorAppStateM :: Functor AppStateM
derive newtype instance applyAppStateM :: Apply AppStateM
derive newtype instance applicativeAppStateM :: Applicative AppStateM
derive newtype instance bindAppStateM :: Bind AppStateM
derive newtype instance monadAppStateM :: Monad AppStateM

get :: AppStateM AppState
get = AppStateM $ do
    ref <- ask
    lift $ read ref

modify :: (AppState -> AppState) -> AppStateM Unit
modify f = AppStateM $ do
    ref <- ask
    lift $ modify_ f ref

put :: AppState -> AppStateM Unit
put a = modify (const a)

run :: forall x. Ref AppState -> AppStateM x -> Effect x
run r (AppStateM f) = runReaderT f r

instance readSymbolsAppStateM :: ReadSymbols AppStateM where
    getSymbols = _.present.symbols <$> get
    getSymbolMap = _.present.symbolMap <$> get

instance writeSymbolsAppStateM :: WriteSymbols AppStateM where
    addSymbol symbol = get >>= \state ->
        case updateMap state.present.symbolMap symbol of
            Left e -> error e
            Right newmap -> modify $ _
                { history = state.history <> [state.present]
                , future = []
                , present = state.present
                    { symbols = state.present.symbols <> [symbol]
                    , symbolMap = newmap
                    }
                , error = Nothing
                }

instance readSequentsAppStateM :: ReadSequents AppStateM where
    getSequents = _.present.sequents <$> get

instance writeSequentsAppStateM :: WriteSequents AppStateM where
    addSequent sequent = do
        state <- get
        modify $ _
            { history = state.history <> [state.present]
            , future = []
            , present = state.present
                { sequents = state.present.sequents <> [sequent] }
            , error = Nothing
            }

instance readProofAppStateM :: ReadProof AppStateM where
    getProof = _.present.proof <$> get

instance writeProofAppStateM :: WriteProof AppStateM where
    addDeduction deduction = get >>= \state ->
        case P.addDeduction deduction state.present.proof of
            Left e -> error e
            Right newproof -> modify $ _
                { history = state.history <> [state.present]
                , future = []
                , present = state.present
                    { proof = newproof }
                , error = Nothing
                }

instance errorAppStateM :: Error AppStateM where
    errors e = modify $ _ { error = Just e }
    clear = modify $ _ { error = Nothing }

instance readErrorAppStateM :: ReadError AppStateM where
    getErrors = _.error <$> get

instance readNavAppStateM :: ReadNav AppStateM where
    getWindow = _.window <$> get

instance navAppStateM :: Nav AppStateM where
    setWindow window = modify $ _ { window = window }

instance historyAppStateM :: History AppStateM where
    undo = get >>= \state -> case A.unsnoc state.history of
        Nothing -> pure unit
        Just {init, last} -> modify $ _
            { history = init
            , future = [state.present] <> state.future
            , present = last
            , error = Nothing
            }
    redo = get >>= \state -> case A.uncons state.future of
        Nothing -> pure unit
        Just {head, tail} -> modify $ _
            { history = state.history <> [state.present]
            , future = tail
            , present = head
            , error = Nothing
            }
    new = do
        state <- get
        modify $ _
            { history = state.history <> [state.present]
            , future = []
            , present =
                { sequents : []
                , symbols : []
                , symbolMap : defaultMap
                , proof : P.empty
                }
            , error = Nothing
            }
