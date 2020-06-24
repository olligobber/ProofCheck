module Symbol
    ( CustomSymbol(..)
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
    , defaultSymbols
    , SymbolMap
    , defaultMap
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
import WFF (WFF(..), UnaryOp, BinaryOp)
import WFF as WFF


-- Custom symbol defined in terms of a wff
data CustomSymbol
    = UnarySymbol { operator :: UnaryOp, definition :: WFF Unit Void Void }
    | BinarySymbol { operator :: BinaryOp, definition :: WFF Boolean Void Void }
    -- true is left, false is right

derive instance eqCustomSymbol :: Eq CustomSymbol
derive instance ordCustomSymbol :: Ord CustomSymbol

-- Alias to other symbol
data SymbolAlias
    = UnaryAlias { name :: String, operator :: UnaryOp }
    | BinaryAlias { name :: String, operator :: BinaryOp }

derive instance eqSymbolAlias :: Eq SymbolAlias
derive instance ordSymbolAlias :: Ord SymbolAlias

-- Builtin symbol
data BuiltinSymbol
    = UnaryBuiltin { operator :: UnaryOp }
    | BinaryBuiltin { operator :: BinaryOp }

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

getDisplay :: Symbol -> String
getDisplay (Custom (UnarySymbol u)) = WFF.renderUnaryOp u.operator
getDisplay (Custom (BinarySymbol b)) = WFF.renderBinaryOp b.operator
getDisplay (Alias (UnaryAlias u)) = WFF.renderUnaryOp u.operator
getDisplay (Alias (BinaryAlias b)) = WFF.renderBinaryOp b.operator
getDisplay (Builtin (UnaryBuiltin u)) = WFF.renderUnaryOp u.operator
getDisplay (Builtin (BinaryBuiltin b)) = WFF.renderBinaryOp b.operator

getTyped :: Symbol -> String
getTyped (Custom (UnarySymbol u)) = WFF.renderUnaryOp u.operator
getTyped (Custom (BinarySymbol b)) = WFF.renderBinaryOp b.operator
getTyped (Alias (UnaryAlias u)) = u.name
getTyped (Alias (BinaryAlias b)) = b.name
getTyped (Builtin (UnaryBuiltin u)) = WFF.renderUnaryOp u.operator
getTyped (Builtin (BinaryBuiltin b)) = WFF.renderBinaryOp b.operator

getOperator :: Symbol -> Either UnaryOp BinaryOp
getOperator (Custom (UnarySymbol u)) = Left u.operator
getOperator (Custom (BinarySymbol b)) = Right b.operator
getOperator (Alias (UnaryAlias u)) = Left u.operator
getOperator (Alias (BinaryAlias b)) = Right b.operator
getOperator (Builtin (UnaryBuiltin u)) = Left u.operator
getOperator (Builtin (BinaryBuiltin b)) = Right b.operator

defaultSymbols :: Array Symbol
defaultSymbols =
    [ Builtin $ UnaryBuiltin { operator : WFF.UnaryOp "~" }
    , Builtin $ BinaryBuiltin { operator : WFF.BinaryOp "∨" }
    , Builtin $ BinaryBuiltin { operator : WFF.BinaryOp "∨" }
    , Builtin $ BinaryBuiltin { operator : WFF.BinaryOp "⇒" }
    , Alias $ BinaryAlias { name : "&", operator : WFF.BinaryOp "∧" }
    , Alias $ BinaryAlias { name : "|", operator : WFF.BinaryOp "∨" }
    , Alias $ BinaryAlias { name : "->", operator : WFF.BinaryOp "⇒" }
    ]

type SymbolMap = Map String (Either UnaryOp BinaryOp)

defaultMap :: SymbolMap
defaultMap = M.fromFoldable
    [ Tuple "~" $ Left $ WFF.UnaryOp "~"
    , Tuple "∧" $ Right $ WFF.BinaryOp "∧"
    , Tuple "∨" $ Right $ WFF.BinaryOp "∨"
    , Tuple "⇒" $ Right $ WFF.BinaryOp "⇒"
    , Tuple "&" $ Right $ WFF.BinaryOp "∧"
    , Tuple "|" $ Right $ WFF.BinaryOp "∨"
    , Tuple "->" $ Right $ WFF.BinaryOp "⇒"
    ]

updateMap :: SymbolMap -> Symbol -> Either String SymbolMap
updateMap m s
    | M.member (getTyped s) m =
        Left $ "Symbol " <> getTyped s <> " is already defined"
    | otherwise =
        Right $ M.insert (getTyped s) (getOperator s) m

makeMap :: forall t. Foldable t => t Symbol -> Either String SymbolMap
makeMap = foldM updateMap M.empty
