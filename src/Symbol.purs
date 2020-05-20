module Symbol
    ( Symbol(..)
    , makeUnary
    , makeBinary
    , SymbolMap
    , defaultMap
    ) where

import Prelude (class Eq, Unit, (==), ($), (<$), (<$>), unit)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

import WFF (UnaryOp, BinaryOp, WFF)
import WFF as WFF

data Symbol
    = UnarySymbol { operator :: UnaryOp, definition :: WFF Unit }
    | BinarySymbol { operator :: BinaryOp, definition :: WFF Boolean }
    -- true is left, false is right

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

type SymbolMap = Map String (Either UnaryOp BinaryOp)

defaultMap :: SymbolMap
defaultMap = M.fromFoldable
    [ Tuple "&" $ Right WFF.andOp
    , Tuple "|" $ Right WFF.orOp
    , Tuple "->" $ Right WFF.impliesOp
    , Tuple "~" $ Left WFF.negOp
    ]
