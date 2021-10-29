module Deduce
    ( class Deduce
    , isAssumption
    , renderRule
    , matchDeduction
    , Reference
    ) where

import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Set (Set)
import Data.Map (Map)

type Reference w =
    { formula :: w
    , isAssumption :: Boolean
    , assumptions :: Set Int
    }

class Deduce r w | r -> w where
    -- Check if the deduction rule introduces an assumption
    isAssumption :: r -> Boolean
    -- Render the deduction rule
    renderRule :: r -> String
    {-
        Check a deduction was correctly applied, given
            - the list of referenced formulas, whether they were assumptions,
                and what assumptions they rely on
            - the map from assumption numbers to formulas
            - the conclusion
            - the deduction rule
        returns either an error or the assumptions the conclusion relies on,
        using Nothing as a flag that this is a new assumption
    -}
    matchDeduction :: Array (Reference w) -> Map Int w -> w -> r ->
        Either String (Maybe (Set Int))
