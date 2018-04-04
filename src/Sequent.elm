module Sequent exposing
    ( Sequent
    , verify
    , match
    , dn1
    , dn2
    , mp
    , mt
    , ai
    , ae1
    , ae2
    , oi1
    , oi2
    )

import WFF exposing (WFF, eval, and, or, implies, neg, variables, match)
import PermTools exposing (assignments, permutations)
import List exposing (foldl, filter, all, map2, map, length)
import Dict exposing (Dict, get, empty)
import Maybe exposing (withDefault, andThen)
import Set exposing (Set, union, empty)
import DictMerge exposing (mergeDef)

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
    (foldl (\w -> \v -> union (variables w) v) Set.empty seq.ante)
    (variables seq.conse)

-- Filters assignments that a wff doesn't satisfy
filterWFF : WFF -> List (Dict String Bool) -> List (Dict String Bool)
filterWFF wff list = filter (satisfies wff) list

-- Check a sequent holds
verify : Sequent -> Bool
verify seq = foldl filterWFF (assignments <| seqVars seq) seq.ante
    |> all (satisfies seq.conse)

-- Match a sequent to one after substitutions were applied, returning
-- the substitutions
match1 : Sequent -> Sequent -> Maybe (Dict String WFF)
match1 small big = foldl
    mergeDef
    (Just Dict.empty)
    (map2 WFFTools.match small.ante big.ante)
    |> mergeDef (WFFTools.match small.conse big.conse)

-- Get any non-nothing value
getAny : List (Maybe a) -> Maybe a
getAny list = case list of
    [] -> Nothing
    Nothing::lists -> getAny lists
    x::_ -> x

-- Match a sequent to one after substitutions were applied and the antecedents
-- permuted, returning the substitutions
match : Sequent -> Sequent -> Maybe (Dict String WFF)
match small big =
    if length small.ante == length big.ante then
        getAny
            <| map
                (\x -> match1 {small | ante = x} big)
                (permutations small.ante)
    else
        Nothing

-- Double Negation sequent 1
dn1 : Sequent
dn1 =
    { ante = [neg (neg (Prop "A"))]
    , conse = Prop "A"
    }

-- Double Negation sequent 2
dn2 : Sequent
dn2 =
    { ante = [Prop "A"]
    , conse = neg (neg (Prop "A"))
    }

-- Modus Ponens sequent
mp : Sequent
mp =
    { ante =
        [ Prop "A"
        , implies (Prop "A") (Prop "B")
        ]
    , conse = Prop "B"
    }

-- Modus Tollens sequent
mt : Sequent
mt =
    { ante =
        [ neg (Prop "B")
        , implies (Prop "A") (Prop "B")
        ]
    , conse = neg (Prop "A")
    }

-- And Introduction sequent
ai : Sequent
ai =
    { ante =
        [ Prop "A"
        , Prop "B"
        ]
    , conse = and (Prop "A") (Prop "B")
    }

-- And Elimination sequent 1
ae1 : Sequent
ae1 =
    { ante = [and (Prop "A") (Prop "B")]
    , conse = Prop "A"
    }

-- And Elimination sequent 2
ae2 : Sequent
ae2 =
    { ante = [and (Prop "A") (Prop "B")]
    , conse = Prop "B"
    }

-- Or Introduction sequent 1
oi1 : Sequent
oi1 =
    { ante = [Prop "A"]
    , conse = or (Prop "A") (Prop "B")
    }

-- Or Introduction sequent 2
oi2 : Sequent
oi2 =
    { ante = [Prop "B"]
    , conse = or (Prop "A") (Prop "B")
    }
