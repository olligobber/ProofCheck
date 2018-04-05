module CustomSymbol exposing
    ( Symbol
    , makeUnary
    , makeBinary
    , toSequent1
    , toSequent2
    )

import WFF exposing (WFF(..), fromUn, fromBin, eval, variables)
import Sequent exposing (Sequent)
import Set exposing (diff, singleton, fromList, isEmpty)

type alias Symbol =
    { wff : WFF
    , definition : WFF
    }

-- Construct a unary symbol, nothing if extra variables found
makeUnary : String -> String -> WFF -> Maybe Symbol
makeUnary prop symbol def =
    if isEmpty <| diff (variables def) (singleton prop) then
        Just
            { wff = Unary
                { function = fromUn
                    (\b -> eval
                        (\c -> case c of
                            prop -> b
                            _ -> True)
                        def)
                , symbol = symbol
                , contents = Prop prop
                }
            , definition = def
            }
    else
        Nothing

-- Construct a binary symbol, nothing if extra variables found
makeBinary : String -> String -> String -> WFF -> Maybe Symbol
makeBinary propa propb symbol def =
    if isEmpty <| diff (variables def) (fromList [propa, propb]) then
        Just
            { wff = Binary
                { function = fromBin
                    (\a -> \b -> eval
                        (\c -> case c of
                            propa -> a
                            propb -> b
                            _ -> True)
                        def)
                , symbol = symbol
                , first = Prop propa
                , second = Prop propb
                }
            , definition = def
            }
    else
        Nothing

-- Turns a symbol definition into the first sequent
toSequent1 : Symbol -> Sequent
toSequent1 symbol =
    { ante = [symbol.wff]
    , conse = symbol.definition
    }

-- Turns a symbol definition into the second sequent
toSequent2 : Symbol -> Sequent
toSequent2 symbol =
    { ante = [symbol.definition]
    , conse = symbol.wff
    }
