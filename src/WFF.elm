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
    )

import List exposing (all)

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
    0 -> \_ -> False
    1 -> identity
    2 -> not
    _ -> \_ -> True

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
