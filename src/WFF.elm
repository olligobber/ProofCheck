module WFF exposing (..)

{-
A Well Formed Formula, either a Proposition, Unary (prefix) operation, or a
Binary (infix) operation
-}
type WFF
    = Prop { char : String }
    | Unary
        { function : Bool -> Bool
        , symbol : String
        , contents : WFF
        }
    | Binary
        { function : Bool -> Bool -> Bool
        , symbol : String
        , first: WFF
        , second : WFF
        }

-- Renders a WFF to display to the user
show : WFF -> String
show wff = case wff of
    Prop v -> v.char
    Unary v -> v.symbol ++ safeShow v.contents
    Binary v -> safeShow v.first ++ v.symbol ++ safeShow v.second

-- Renders a WFF to be contained in another WFF
safeShow : WFF -> String
safeShow wff = case wff of
    Binary v -> "(" ++ show (Binary v) ++ ")"
    v -> show v

-- Given a mapping of propositions to values, evaluates a WFF
eval : (String -> Bool) -> WFF -> Bool
eval mapping wff = case wff of
    Prop v -> mapping v.char
    Unary v -> v.function <| eval mapping v.contents
    Binary v -> v.function (eval mapping v.first) (eval mapping v.second)

-- Unary operator "not"
neg : WFF -> WFF
neg v = Unary
    { function = not
    , symbol = "~"
    , contents = v
    }

-- Binary operator "and"
and : WFF -> WFF -> WFF
and a b = Binary
    { function = (&&)
    , symbol = "&"
    , first = a
    , second = b
    }

-- Binary operator "or"
or : WFF -> WFF -> WFF
or a b = Binary
    { function = (||)
    , symbol = "|"
    , first = a
    , second = b
    }

-- Binary operator "implies"
implies : WFF -> WFF -> WFF
implies a b = Binary
    { function = (not >> (||))
    , symbol = "->"
    , first = a
    , second = b
    }
