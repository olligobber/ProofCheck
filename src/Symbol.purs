module Symbol
    ( Symbol(..)
    , makeUnary
    , makeBinary
    , toSequents
    , getName
    , SymbolMap
    , defaultMap
    , updateMap
    ) where

import Prelude
    (class Eq, class Ord, Unit, (==), ($), (<$), (<$>), (<>), unit, otherwise)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

import WFF (UnaryOp, BinaryOp, WFF(..))
import WFF as WFF
import Sequent (Sequent(..))

data Symbol
    = UnarySymbol { operator :: UnaryOp, definition :: WFF Unit }
    | BinarySymbol { operator :: BinaryOp, definition :: WFF Boolean }
    -- true is left, false is right

derive instance eqSymbol :: Eq Symbol
derive instance ordSymbol :: Ord Symbol

makeUnary :: forall a. Eq a => a -> String -> WFF a -> Either String Symbol
makeUnary p s w = case renamed of
    Just definition -> Right $ UnarySymbol
        { operator : WFF.makeUnary (\b -> WFF.eval $ b <$ definition) s
        , definition
        }
    Nothing -> Left "Extra proposition(s) found in symbol definition"
    where
        renamed = traverse (\q -> if p == q then Just unit else Nothing) w

makeBinary :: forall a. Eq a =>
    a -> a -> String -> WFF a -> Either String Symbol
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

toSequents :: Symbol -> Array (Sequent Int)
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

getName :: Symbol -> String
getName (UnarySymbol u) = u.operator.symbol
getName (BinarySymbol b) = b.operator.symbol

type SymbolMap = Map String (Either UnaryOp BinaryOp)

defaultMap :: SymbolMap
defaultMap = M.fromFoldable
    [ Tuple "&" $ Right WFF.andOp
    , Tuple "|" $ Right WFF.orOp
    , Tuple "->" $ Right WFF.impliesOp
    , Tuple "~" $ Left WFF.negOp
    ]

updateMap :: SymbolMap -> Symbol -> Either String SymbolMap
updateMap m (UnarySymbol u)
    | M.member u.operator.symbol m =
        Left $ "Symbol " <> u.operator.symbol <> " is already defined"
    | otherwise = Right $ M.insert u.operator.symbol (Left u.operator) m
updateMap m (BinarySymbol b)
    | M.member b.operator.symbol m =
        Left $ "Symbol " <> b.operator.symbol <> " is already defined"
    | otherwise = Right $ M.insert b.operator.symbol (Right b.operator) m
