module WFF exposing
    ( WFF(..)
    , show
    , fromUn
    , fromBin
    , eval
    , neg
    , and
    , or
    , implies
    , variables
    , substitute
    , match
    )

import Set exposing (Set, singleton, union)
import Dict exposing (Dict, singleton)
import DictMerge exposing (mergeErr)

{-
A Well Formed Formula, either a Proposition, Unary (prefix) operation, or a
Binary (infix) operation
-}
type WFF
    = Prop String
    | Unary
        { function : Int
        , symbol : String
        , contents : WFF
        }
    | Binary
        { function : Int
        , symbol : String
        , first: WFF
        , second : WFF
        }

-- Renders a WFF to display to the user
show : WFF -> String
show wff = case wff of
    Prop v -> v
    Unary v -> v.symbol ++ safeShow v.contents
    Binary v -> safeShow v.first ++ v.symbol ++ safeShow v.second

-- Renders a WFF to be contained in another WFF
safeShow : WFF -> String
safeShow wff = case wff of
    Prop v -> v
    v -> "(" ++ show v ++ ")"

-- Turns an int into a unary operator
toUn : Int -> Bool -> Bool
toUn n = case (n%4) of
    0 -> always False
    1 -> identity
    2 -> not
    _ -> always True

-- Turns a unary operator into an int
fromUn : (Bool -> Bool) -> Int
fromUn f = case (f False, f True) of
    (False, False) -> 0
    (False, True) -> 1
    (True, False) -> 2
    (True, True) -> 3

-- Turns an int into a binary operator
toBin : Int -> Bool -> Bool -> Bool
toBin n a = case a of
    True -> toUn (n//4)
    False -> toUn (n%4)

-- Turns a binary operator into an int
fromBin : (Bool -> Bool -> Bool) -> Int
fromBin f = fromUn (f False) + 4 * fromUn (f True)

-- Given a mapping of propositions to values, evaluates a WFF
eval : (String -> Bool) -> WFF -> Bool
eval mapping wff = case wff of
    Prop v -> mapping v
    Unary v -> toUn v.function <| eval mapping v.contents
    Binary v -> toBin v.function (eval mapping v.first) (eval mapping v.second)

-- Unary operator "not"
neg : WFF -> WFF
neg v = Unary
    { function = fromUn not
    , symbol = "~"
    , contents = v
    }

-- Binary operator "and"
and : WFF -> WFF -> WFF
and a b = Binary
    { function = fromBin (&&)
    , symbol = "&"
    , first = a
    , second = b
    }

-- Binary operator "or"
or : WFF -> WFF -> WFF
or a b = Binary
    { function = fromBin (||)
    , symbol = "|"
    , first = a
    , second = b
    }

-- Binary operator "implies"
implies : WFF -> WFF -> WFF
implies a b = Binary
    { function = fromBin (not >> (||))
    , symbol = "->"
    , first = a
    , second = b
    }

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

-- Match a WFF to one after substitutions were applied, returning the relevant
-- substitutions
match : WFF -> WFF -> Maybe (Dict String WFF)
match small big = case (small, big) of
    (Binary v, Binary u) ->
        if v.symbol == u.symbol && v.function == u.function then
            case (match v.first u.first, match v.second u.second) of
                (Just d1, Just d2) -> mergeErr d1 d2
                _ -> Nothing
        else
            Nothing
    (Unary v, Unary u) ->
        if v.symbol == u.symbol && v.function == u.function then
            match v.contents u.contents
        else
            Nothing
    (Prop s, _) -> Just <| Dict.singleton s big
    _ -> Nothing
