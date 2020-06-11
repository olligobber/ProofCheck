module Symbol
    ( CustomSymbol(..)
    , SymbolAlias(..)
    , BuiltinSymbol(..)
    , Symbol(..)
    , makeUnary
    , makeBinary
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

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldM)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Prelude (class Eq, class Ord, Unit, (==), ($), (<$), (<$>), (<>), unit, otherwise)
import Sequent (Sequent(..))
import WFF (UnaryOp, BinaryOp, WFF(..))
import WFF as WFF
import Data.Tuple (Tuple(..))

-- Custom symbol defined in terms of a wff
data CustomSymbol
    = UnarySymbol { operator :: UnaryOp, definition :: WFF Unit }
    | BinarySymbol { operator :: BinaryOp, definition :: WFF Boolean }
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
makeUnary :: forall a. Eq a => a -> String -> WFF a -> Either String CustomSymbol
makeUnary p s w = case renamed of
    Just definition -> Right $ UnarySymbol
        { operator : WFF.makeUnary (\b -> WFF.eval $ b <$ definition) s
        , definition
        }
    Nothing -> Left "Extra proposition(s) found in symbol definition"
    where
        renamed = traverse (\q -> if p == q then Just unit else Nothing) w

-- Make a custom binary symbol
makeBinary :: forall a. Eq a =>
    a -> a -> String -> WFF a -> Either String CustomSymbol
makeBinary p q s w = case renamed of
    Just definition -> Right $ BinarySymbol
        { operator : WFF.makeBinary
            (\a b -> WFF.eval $ (if _ then a else b) <$> definition)
            s
        , definition
        }
    Nothing -> Left "Extra proposition(s) found in symbol definition"
    where
        renamed = traverse
            (\r ->
                if p == r then Just true
                else if q == r then Just false
                else Nothing)
            w

toSequents :: CustomSymbol -> Array (Sequent Int)
toSequents (UnarySymbol s) =
    [ Sequent { ante : [ withOp ], conse : noOp }
    , Sequent { ante : [ noOp ], conse : withOp }
    ]
    where
        withOp = Unary {operator : s.operator, contents: Prop 1}
        noOp = 1 <$ s.definition
toSequents (BinarySymbol s) =
    [ Sequent { ante : [ withOp ], conse : noOp }
    , Sequent { ante : [ noOp ], conse : withOp }
    ]
    where
        withOp = Binary {operator : s.operator, left : Prop 1, right : Prop 0}
        noOp = (if _ then 1 else 0) <$> s.definition

getDisplay :: Symbol -> String
getDisplay (Custom (UnarySymbol u)) = u.operator.symbol
getDisplay (Custom (BinarySymbol b)) = b.operator.symbol
getDisplay (Alias (UnaryAlias u)) = u.operator.symbol
getDisplay (Alias (BinaryAlias b)) = b.operator.symbol
getDisplay (Builtin (UnaryBuiltin u)) = u.operator.symbol
getDisplay (Builtin (BinaryBuiltin b)) = b.operator.symbol

getTyped :: Symbol -> String
getTyped (Custom (UnarySymbol u)) = u.operator.symbol
getTyped (Custom (BinarySymbol b)) = b.operator.symbol
getTyped (Alias (UnaryAlias u)) = u.name
getTyped (Alias (BinaryAlias b)) = b.name
getTyped (Builtin (UnaryBuiltin u)) = u.operator.symbol
getTyped (Builtin (BinaryBuiltin b)) = b.operator.symbol

getOperator :: Symbol -> Either UnaryOp BinaryOp
getOperator (Custom (UnarySymbol u)) = Left u.operator
getOperator (Custom (BinarySymbol b)) = Right b.operator
getOperator (Alias (UnaryAlias u)) = Left u.operator
getOperator (Alias (BinaryAlias b)) = Right b.operator
getOperator (Builtin (UnaryBuiltin u)) = Left u.operator
getOperator (Builtin (BinaryBuiltin b)) = Right b.operator

defaultSymbols :: Array Symbol
defaultSymbols =
    [ Builtin $ UnaryBuiltin { operator : WFF.negOp }
    , Builtin $ BinaryBuiltin { operator : WFF.orOp }
    , Builtin $ BinaryBuiltin { operator : WFF.andOp }
    , Builtin $ BinaryBuiltin { operator : WFF.impliesOp }
    , Alias $ BinaryAlias { name : "&", operator : WFF.andOp }
    , Alias $ BinaryAlias { name : "|", operator : WFF.orOp }
    , Alias $ BinaryAlias { name : "->", operator : WFF.impliesOp }
    ]

type SymbolMap = Map String (Either UnaryOp BinaryOp)

defaultMap :: SymbolMap
defaultMap = M.fromFoldable
    [ Tuple "~" $ Left WFF.negOp
    , Tuple "∧" $ Right WFF.andOp
    , Tuple "∨" $ Right WFF.orOp
    , Tuple "⇒" $ Right WFF.impliesOp
    , Tuple "&" $ Right WFF.andOp
    , Tuple "|" $ Right WFF.orOp
    , Tuple "->" $ Right WFF.impliesOp
    ]

updateMap :: SymbolMap -> Symbol -> Either String SymbolMap
updateMap m s
    | M.member (getTyped s) m =
        Left $ "Symbol " <> getTyped s <> " is already defined"
    | otherwise =
        Right $ M.insert (getTyped s) (getOperator s) m

makeMap :: forall t. Foldable t => t Symbol -> Either String SymbolMap
makeMap = foldM updateMap M.empty
