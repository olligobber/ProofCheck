module Sequent exposing
    ( Sequent
    , verify )

import WFF exposing (WFF, eval)
import WFFTools exposing (variables, match)
import PermTools exposing (assignments, permutations)
import List exposing (foldl, filter, all)
import Dict exposing (Dict, get)
import Maybe exposing (withDefault)
import Set exposing (Set, union, empty)

type alias Sequent =
    { ante : List WFF
    , conse : WFF
    }

-- Check a wff satisfies an assignment
satisfies : WFF -> Dict String Bool -> Bool
satisfies wff assign = eval (\s -> withDefault False (get s assign)) wff

-- Gets all variables from a sequent
seqVars : Sequent -> Set String
seqVars seq = union
    (foldl (\w -> \v -> union (variables w) v) empty seq.ante)
    (variables seq.conse)

-- Filters assignments that a wff doesn't satisfy
filterWFF : WFF -> List (Dict String Bool) -> List (Dict String Bool)
filterWFF wff list = filter
    (satisfies wff)
    list

-- Check a sequent holds
verify : Sequent -> Bool
verify seq = all (satisfies seq.conse) <|
    foldl filterWFF (assignments <| seqVars seq) seq.ante
