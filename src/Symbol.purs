module Symbol
    ( Operator(..)
    , CustomSymbol(..)
    , SymbolAlias(..)
    , BuiltinSymbol(..)
    , Symbol(..)
    , makeUnary
    , makeBinary
    , renderableUnary
    , renderableBinary
    , toSequents
    , getDisplay
    , getTyped
    , getOperator
    , oldDefaultSymbols
    , newDefaultSymbols
    , SymbolMap
    , oldDefaultMap
    , newDefaultMap
    , updateMap
    , makeMap
    ) where

import Prelude
    ( class Eq, class Ord
    , Unit
    , (==), ($), (<>), (>>=), (>=>), (<<<)
    , unit, otherwise, const
    )
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void, absurd)
import Data.Identity (Identity(..))

import Sequent (Sequent(..))
import WFF (WFF(..), UnaryOp, BinaryOp, Quantifier)
import WFF as WFF

data Operator
    = QuantOperator Quantifier
    | UnaryOperator UnaryOp
    | BinaryOperator BinaryOp

derive instance eqOperator :: Eq Operator
derive instance ordOperator :: Ord Operator

-- Custom symbol defined in terms of a wff
data CustomSymbol
    = UnarySymbol { operator :: UnaryOp, definition :: WFF Unit Void Void }
    | BinarySymbol { operator :: BinaryOp, definition :: WFF Boolean Void Void }
    -- true is left, false is right

derive instance eqCustomSymbol :: Eq CustomSymbol
derive instance ordCustomSymbol :: Ord CustomSymbol

-- Alias to other symbol
type SymbolAlias = { name :: String, operator :: Operator }

-- Builtin symbol
newtype BuiltinSymbol = BuiltinSymbol Operator

derive instance eqBuiltinSymbol :: Eq BuiltinSymbol
derive instance ordBuiltinSymbol :: Ord BuiltinSymbol

data Symbol
    = Custom CustomSymbol
    | Alias SymbolAlias
    | Builtin BuiltinSymbol

derive instance eqSymbol :: Eq Symbol
derive instance ordSymbol :: Ord Symbol

-- Make a custom unary symbol
makeUnary :: forall a b c. Eq a =>
    a -> String -> WFF a b c -> Either String CustomSymbol
makeUnary p s w = case removedVars of
    Nothing -> Left "Variable(s) found in symbol definition"
    Just withoutVars -> case rename withoutVars of
        Nothing -> Left "Extra proposition(s) found in symbol definition"
        Just definition -> Right $ UnarySymbol
            { operator : WFF.UnaryOp s
            , definition
            }
    where
        removedVars = WFF.traverseFree (const Nothing) w
            >>= WFF.traverseBound (const Nothing)
        rename = WFF.traversePredicates
            (\q -> if p == q then Just unit else Nothing)

-- Make a custom binary symbol
makeBinary :: forall a b c. Eq a =>
    a -> a -> String -> WFF a b c -> Either String CustomSymbol
makeBinary p q s w = case removedVars of
    Nothing -> Left "Variable(s) found in symbol definition"
    Just withoutVars -> case rename withoutVars of
        Nothing -> Left "Extra proposition(s) found in symbol definition"
        Just definition -> Right $ BinarySymbol
            { operator : WFF.BinaryOp s
            , definition
            }
    where
        removedVars = WFF.traverseFree (const Nothing) w
            >>= WFF.traverseBound (const Nothing)
        rename = WFF.traversePredicates
            (\r ->
                if p == r then Just true
                else if q == r then Just false
                else Nothing)

fromIdentity :: forall a. Identity a -> a
fromIdentity (Identity x) = x

renderableUnary :: forall a b. WFF Unit Void Void -> WFF String a b
renderableUnary = fromIdentity <<<
    ( WFF.traversePredicates (const $ Identity "A")
    >=> WFF.traverseFree absurd
    >=> WFF.traverseBound absurd
    )

renderableBinary :: forall a b. WFF Boolean Void Void -> WFF String a b
renderableBinary = fromIdentity <<<
    ( WFF.traversePredicates (if _ then Identity "A" else Identity "B")
    >=> WFF.traverseFree absurd
    >=> WFF.traverseBound absurd
    )

toSequents :: forall a b. CustomSymbol -> Array (Sequent String a b)
toSequents (UnarySymbol s) =
    [ Sequent { ante : [ withOp ], conse : noOp }
    , Sequent { ante : [ noOp ], conse : withOp }
    ]
    where
        withOp = Unary {operator : s.operator, contents: WFF.prop "A"}
        noOp = renderableUnary s.definition
toSequents (BinarySymbol s) =
    [ Sequent { ante : [ withOp ], conse : noOp }
    , Sequent { ante : [ noOp ], conse : withOp }
    ]
    where
        withOp = Binary
            { operator : s.operator
            , left : WFF.prop "A"
            , right : WFF.prop "B"
            }
        noOp = renderableBinary s.definition

getOperator :: Symbol -> Operator
getOperator (Custom (UnarySymbol u)) = UnaryOperator u.operator
getOperator (Custom (BinarySymbol b)) = BinaryOperator b.operator
getOperator (Alias a) = a.operator
getOperator (Builtin (BuiltinSymbol o)) = o

getDisplay :: Operator -> String
getDisplay (UnaryOperator u) = WFF.renderUnaryOp u
getDisplay (BinaryOperator b) = WFF.renderBinaryOp b
getDisplay (QuantOperator q) = WFF.renderQ q

getTyped :: Symbol -> String
getTyped (Alias a) = a.name
getTyped s = getDisplay $ getOperator s

oldDefaultSymbols :: Array Symbol
oldDefaultSymbols =
    [ Builtin $ BuiltinSymbol $ UnaryOperator WFF.negOp
    , Builtin $ BuiltinSymbol $ BinaryOperator WFF.andOp
    , Builtin $ BuiltinSymbol $ BinaryOperator WFF.orOp
    , Builtin $ BuiltinSymbol $ BinaryOperator WFF.impliesOp
    , Builtin $ BuiltinSymbol $ QuantOperator WFF.Forall
    , Builtin $ BuiltinSymbol $ QuantOperator WFF.Exists
    , Alias { name : "&", operator : BinaryOperator WFF.andOp }
    , Alias { name : "|", operator : BinaryOperator WFF.orOp }
    , Alias { name : "->", operator : BinaryOperator WFF.impliesOp }
    ]

newDefaultSymbols :: Array Symbol
newDefaultSymbols = oldDefaultSymbols <>
    [ Alias { name : "@", operator : QuantOperator WFF.Forall }
    , Alias { name : "!", operator : QuantOperator WFF.Exists }
    ]

type SymbolMap = Map String Operator

oldDefaultMap :: SymbolMap
oldDefaultMap = M.fromFoldable
    [ Tuple "~" $ UnaryOperator WFF.negOp
    , Tuple "∧" $ BinaryOperator WFF.andOp
    , Tuple "∨" $ BinaryOperator WFF.orOp
    , Tuple "⇒" $ BinaryOperator WFF.impliesOp
    , Tuple "&" $ BinaryOperator WFF.andOp
    , Tuple "|" $ BinaryOperator WFF.orOp
    , Tuple "->" $ BinaryOperator WFF.impliesOp
    , Tuple "∀" $ QuantOperator WFF.Forall
    , Tuple "∃" $ QuantOperator WFF.Exists
    ]

newDefaultMap :: SymbolMap
newDefaultMap = oldDefaultMap <> M.fromFoldable
    [ Tuple "@" $ QuantOperator WFF.Forall
    , Tuple "!" $ QuantOperator WFF.Exists
    ]

updateMap :: SymbolMap -> Symbol -> Either String SymbolMap
updateMap m s
    | M.member (getTyped s) m =
        Left $ "Symbol " <> getTyped s <> " is already defined"
    | otherwise =
        Right $ M.insert (getTyped s) (getOperator s) m

makeMap :: forall t. Foldable t => t Symbol -> Either String SymbolMap
makeMap = foldM updateMap M.empty
