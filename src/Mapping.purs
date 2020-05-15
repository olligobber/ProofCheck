module Mapping (Mapping(..)) where

import Prelude
    ( class Eq, class Ord, class Semigroup, class Monoid
    , (==), ($)
    , identity
    )
import Data.Foldable (all)
import Data.Map (Map)
import Data.Map as M

data Mapping k v = None | Mapping (Map k v)

instance semigroupMapping :: (Ord k, Eq v) => Semigroup (Mapping k v) where
    append None _ = None
    append _ None = None
    append (Mapping m) (Mapping n) =
        if all identity $ M.intersectionWith (==) m n then
            Mapping $ M.union m n
        else
            None

instance monoidMapping :: (Ord k, Eq v) => Monoid (Mapping k v) where
    mempty = Mapping $ M.empty
