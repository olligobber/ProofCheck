module WFFTools exposing
    ( variables
    , substitute
    , match
    )

import Set exposing (Set, singleton, union)
import Dict exposing (Dict, empty, insert, merge, singleton)
import Maybe exposing (Maybe, map)
import WFF exposing (WFF(..))

-- Get all variables from a WFF
variables : WFF -> Set String
variables wff = case wff of
    Prop str -> Set.singleton str
    Unary val -> variables val.contents
    Binary val -> union (variables val.first) (variables val.second)

-- Substitute variables for WFF
substitute : (String -> Maybe WFF) -> WFF -> WFF
substitute func wff = case wff of
    Prop str -> case func str of
        Nothing -> Prop str
        Just val -> val
    Unary val -> Unary { val | contents = substitute func val.contents }
    Binary val -> Binary
        { val
        | first = substitute func val.first
        , second = substitute func val.second
        }

-- Merges two dictionaries, returning Nothing on any clash
mergeErr : Dict String WFF -> Dict String WFF -> Maybe (Dict String WFF)
mergeErr a b = merge
    (\k -> \v -> \d -> map (insert k v) d)
    (\k -> \v1 -> \v2 -> \d -> case v1 == v2 of
        True -> map (insert k v1) d
        False -> Nothing)
    (\k -> \v -> \d -> map (insert k v) d)
    a
    b
    (Just empty)

-- Match a WFF to one after substitutions were applied, returning the relevant substitutions
match : WFF -> WFF -> Maybe (Dict String WFF)
match small big = case (small, big) of
    (Binary v, Binary u) ->
        if v.symbol == u.symbol then
            case (match v.first u.first, match v.second u.second) of
                (Just d1, Just d2) -> mergeErr d1 d2
                _ -> Nothing
        else
            Nothing
    (Unary v, Unary u) ->
        if v.symbol == u.symbol then
            match v.contents u.contents
        else
            Nothing
    (Prop s, _) -> Just <| Dict.singleton s big
    _ -> Nothing
